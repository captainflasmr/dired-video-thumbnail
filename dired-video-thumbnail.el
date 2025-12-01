;;; dired-video-thumbnail.el --- Display video thumbnails from dired -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Dyer

;; Author: James Dyer
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: multimedia, files, dired
;; URL: https://github.com/yourusername/dired-video-thumbnail

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides image-dired style thumbnail viewing for video files.
;; It uses ffmpeg to extract thumbnails and displays them in a grid layout.
;;
;; Features:
;; - Persistent thumbnail caching (thumbnails are generated once and reused)
;; - Async thumbnail generation (Emacs remains responsive)
;; - Grid layout display similar to image-dired
;; - Click to play with customisable video player
;; - Works with marked files or all videos in directory
;;
;; Requirements:
;; - ffmpeg must be installed and in your PATH
;;
;; Usage:
;; In a dired buffer, call `dired-video-thumbnail' to display thumbnails
;; for all video files (or marked files if any are marked).
;;
;; Keybindings in the thumbnail buffer:
;; - RET / mouse-1: Play video
;; - g: Regenerate thumbnail for video at point
;; - G: Regenerate all thumbnails
;; - d: Open dired at video's directory
;; - q: Quit thumbnail buffer
;; - n/p: Next/previous video
;; - +/-: Increase/decrease thumbnail size

;;; Code:

(require 'dired)
(require 'image)

;;; Customisation

(defgroup dired-video-thumbnail nil
  "Display video thumbnails from dired."
  :group 'dired
  :prefix "dired-video-thumbnail-")

(defcustom dired-video-thumbnail-cache-dir
  (expand-file-name "dired-video-thumbnails" user-emacs-directory)
  "Directory for caching video thumbnails."
  :type 'directory
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-size 200
  "Width of generated thumbnails in pixels.
Height is calculated automatically to maintain aspect ratio."
  :type 'integer
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-display-height 150
  "Display height of thumbnails in the buffer.
Set to nil to use actual thumbnail size."
  :type '(choice integer (const nil))
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-columns 4
  "Number of thumbnail columns in the display buffer."
  :type 'integer
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-timestamp "00:00:05"
  "Timestamp to extract thumbnail from (HH:MM:SS format).
Set to nil to let ffmpeg choose automatically."
  :type '(choice string (const nil))
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-video-player "mpv"
  "Command to use for playing videos.
Set to nil to use `browse-url-xdg-open' or system default."
  :type '(choice string (const nil))
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-video-extensions
  '("mp4" "mkv" "avi" "mov" "webm" "m4v" "wmv" "flv" "mpeg" "mpg" "ogv" "3gp")
  "List of video file extensions to recognise."
  :type '(repeat string)
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-ffmpeg-program "ffmpeg"
  "Path to ffmpeg executable."
  :type 'string
  :group 'dired-video-thumbnail)

(defcustom dired-video-thumbnail-mark-border-width 4
  "Width of the border around marked thumbnails in pixels."
  :type 'integer
  :group 'dired-video-thumbnail)

(defface dired-video-thumbnail-mark
  '((t :foreground "red"))
  "Face for the border around marked video thumbnails."
  :group 'dired-video-thumbnail)

;;; Internal variables

(defvar dired-video-thumbnail--processes nil
  "List of active thumbnail generation processes.")

(defvar dired-video-thumbnail--pending nil
  "List of videos pending thumbnail generation.")

(defvar dired-video-thumbnail--current-videos nil
  "List of videos in the current thumbnail buffer.")

(defvar dired-video-thumbnail--source-dir nil
  "Source directory for the current thumbnail buffer.")

(defvar-local dired-video-thumbnail--dired-buffer nil
  "The dired buffer associated with this thumbnail buffer.")

(defvar-local dired-video-thumbnail--video-at-point nil
  "Video file path at the current position.")

;;; Utility functions

(defun dired-video-thumbnail--ensure-cache-dir ()
  "Ensure the thumbnail cache directory exists."
  (unless (file-directory-p dired-video-thumbnail-cache-dir)
    (make-directory dired-video-thumbnail-cache-dir t)))

(defun dired-video-thumbnail--video-p (file)
  "Return non-nil if FILE is a video file."
  (and (file-regular-p file)
       (member (downcase (or (file-name-extension file) ""))
               dired-video-thumbnail-video-extensions)))

(defun dired-video-thumbnail--file-marked-p (file)
  "Return non-nil if FILE is marked in the associated dired buffer."
  (when (and dired-video-thumbnail--dired-buffer
             (buffer-live-p dired-video-thumbnail--dired-buffer))
    (with-current-buffer dired-video-thumbnail--dired-buffer
      (save-excursion
        (goto-char (point-min))
        (when (dired-goto-file file)
          (beginning-of-line)
          (looking-at-p dired-re-mark))))))

(defun dired-video-thumbnail--mark-in-dired (file mark)
  "Set MARK on FILE in the associated dired buffer.
MARK should be ?* to mark or ?\\s (space) to unmark."
  (if (not (and dired-video-thumbnail--dired-buffer
                (buffer-live-p dired-video-thumbnail--dired-buffer)))
      (message "No live dired buffer associated!")
    (with-current-buffer dired-video-thumbnail--dired-buffer
      (save-excursion
        (goto-char (point-min))
        (if (not (dired-goto-file file))
            (message "Could not find file in dired: %s" file)
          (let ((inhibit-read-only t))
            (beginning-of-line)
            (delete-char 1)
            (insert-char mark)))))))

(defun dired-video-thumbnail-debug ()
  "Show debug info about current state."
  (interactive)
  (message "Dired buffer: %s (live: %s), Source dir: %s, Videos: %d, Current file: %s"
           dired-video-thumbnail--dired-buffer
           (and dired-video-thumbnail--dired-buffer
                (buffer-live-p dired-video-thumbnail--dired-buffer))
           dired-video-thumbnail--source-dir
           (length dired-video-thumbnail--current-videos)
           (get-text-property (point) 'dired-video-thumbnail-file)))

(defun dired-video-thumbnail--cache-path (video-file)
  "Return the cached thumbnail path for VIDEO-FILE."
  (dired-video-thumbnail--ensure-cache-dir)
  (let* ((full-path (expand-file-name video-file))
         (attrs (file-attributes full-path))
         (mtime (format-time-string "%Y%m%d%H%M%S"
                                    (file-attribute-modification-time attrs)))
         (hash (md5 (concat full-path mtime))))
    (expand-file-name (concat hash ".jpg") dired-video-thumbnail-cache-dir)))

(defun dired-video-thumbnail--cached-p (video-file)
  "Return non-nil if VIDEO-FILE has a cached thumbnail."
  (let ((cache-path (dired-video-thumbnail--cache-path video-file)))
    (and (file-exists-p cache-path)
         (> (file-attribute-size (file-attributes cache-path)) 0))))

;;; Thumbnail generation

(defun dired-video-thumbnail--generate-sync (video-file)
  "Generate thumbnail for VIDEO-FILE synchronously.
Returns the thumbnail path or nil on failure."
  (let ((thumb-path (dired-video-thumbnail--cache-path video-file))
        (args (list "-i" video-file
                    "-vframes" "1"
                    "-vf" (format "scale=%d:%d:force_original_aspect_ratio=decrease,pad=%d:%d:(ow-iw)/2:(oh-ih)/2:black"
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size)
                    "-y")))
    (when dired-video-thumbnail-timestamp
      (setq args (append (list "-ss" dired-video-thumbnail-timestamp) args)))
    (setq args (append args (list thumb-path)))
    (apply #'call-process dired-video-thumbnail-ffmpeg-program nil nil nil args)
    (when (and (file-exists-p thumb-path)
               (> (file-attribute-size (file-attributes thumb-path)) 0))
      thumb-path)))

(defun dired-video-thumbnail--generate-async (video-file callback)
  "Generate thumbnail for VIDEO-FILE asynchronously.
Call CALLBACK with the thumbnail path when done, or nil on failure."
  (let ((thumb-path (dired-video-thumbnail--cache-path video-file))
        (args (list "-i" video-file
                    "-vframes" "1"
                    "-vf" (format "scale=%d:%d:force_original_aspect_ratio=decrease,pad=%d:%d:(ow-iw)/2:(oh-ih)/2:black"
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size
                                  dired-video-thumbnail-size)
                    "-y")))
    (when dired-video-thumbnail-timestamp
      (setq args (append (list "-ss" dired-video-thumbnail-timestamp) args)))
    (setq args (append args (list thumb-path)))
    (let ((proc (make-process
                 :name (format "video-thumb-%s" (file-name-nondirectory video-file))
                 :command (cons dired-video-thumbnail-ffmpeg-program args)
                 :sentinel (lambda (process _event)
                             (when (eq (process-status process) 'exit)
                               (setq dired-video-thumbnail--processes
                                     (delq process dired-video-thumbnail--processes))
                               (if (and (= (process-exit-status process) 0)
                                        (file-exists-p thumb-path)
                                        (> (file-attribute-size
                                            (file-attributes thumb-path)) 0))
                                   (funcall callback thumb-path)
                                 (funcall callback nil)))))))
      (push proc dired-video-thumbnail--processes)
      proc)))

;;; Display functions

(defun dired-video-thumbnail--create-placeholder ()
  "Create a placeholder image for videos without thumbnails."
  (let* ((size dired-video-thumbnail-size)
         (svg (format
               "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>
                  <rect width='100%%' height='100%%' fill='#333'/>
                  <text x='50%%' y='50%%' fill='#999' font-size='14'
                        text-anchor='middle' dominant-baseline='middle'>
                    Loading...
                  </text>
                </svg>"
               size size)))
    (create-image svg 'svg t
                  :height dired-video-thumbnail-display-height)))

(defun dired-video-thumbnail--create-error-placeholder ()
  "Create a placeholder image for failed thumbnails."
  (let* ((size dired-video-thumbnail-size)
         (svg (format
               "<svg xmlns='http://www.w3.org/2000/svg' width='%d' height='%d'>
                  <rect width='100%%' height='100%%' fill='#433'/>
                  <text x='50%%' y='50%%' fill='#c99' font-size='12'
                        text-anchor='middle' dominant-baseline='middle'>
                    No thumbnail
                  </text>
                </svg>"
               size size)))
    (create-image svg 'svg t
                  :height dired-video-thumbnail-display-height)))

(defun dired-video-thumbnail--create-bordered-image (image-path marked)
  "Create an image from IMAGE-PATH with optional border if MARKED."
  (if (not marked)
      (create-image image-path nil nil
                    :height dired-video-thumbnail-display-height)
    ;; Create SVG wrapper with border around the image
    (let* ((border-width dired-video-thumbnail-mark-border-width)
           (border-color (face-foreground 'dired-video-thumbnail-mark nil t))
           (size dired-video-thumbnail-size)
           (total-size (+ size (* 2 border-width)))
           ;; Read and base64 encode the image
           (image-data (with-temp-buffer
                         (insert-file-contents-literally image-path)
                         (base64-encode-string (buffer-string) t)))
           (svg (format
                 "<svg xmlns='http://www.w3.org/2000/svg' 
                       xmlns:xlink='http://www.w3.org/1999/xlink'
                       width='%d' height='%d'>
                    <rect x='0' y='0' width='%d' height='%d' fill='%s'/>
                    <image x='%d' y='%d' width='%d' height='%d'
                           xlink:href='data:image/jpeg;base64,%s'/>
                  </svg>"
                 total-size total-size
                 total-size total-size border-color
                 border-width border-width size size
                 image-data)))
      (create-image svg 'svg t
                    :height dired-video-thumbnail-display-height))))

(defun dired-video-thumbnail--insert-thumbnail (video-file &optional thumb-path marked)
  "Insert thumbnail for VIDEO-FILE at point.
THUMB-PATH is the path to the thumbnail image, or nil for placeholder.
MARKED if non-nil shows the thumbnail as marked with a border."
  (let* ((name (file-name-nondirectory video-file))
         (image (cond
                 ((and thumb-path (file-exists-p thumb-path))
                  (dired-video-thumbnail--create-bordered-image thumb-path marked))
                 ((eq thumb-path :error)
                  (dired-video-thumbnail--create-error-placeholder))
                 (t (dired-video-thumbnail--create-placeholder))))
         (start (point)))
    (insert-image image)
    (put-text-property start (point) 'dired-video-thumbnail-file video-file)
    (put-text-property start (point) 'keymap dired-video-thumbnail-item-map)
    (put-text-property start (point) 'help-echo (format "%s%s\nClick to play, m to mark"
                                                        (if marked "[MARKED] " "")
                                                        name))))

(defun dired-video-thumbnail--update-thumbnail (video-file thumb-path)
  "Update the display for VIDEO-FILE with THUMB-PATH."
  (when-let ((buf (get-buffer "*Video Thumbnails*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (when-let ((file (get-text-property (point) 'dired-video-thumbnail-file)))
              (when (string= file video-file)
                (let ((start (point))
                      (end (next-single-property-change (point)
                                                        'dired-video-thumbnail-file
                                                        nil (point-max))))
                  (delete-region start end)
                  (dired-video-thumbnail--insert-thumbnail
                   video-file
                   (or thumb-path :error)))))
            (goto-char (or (next-single-property-change (point)
                                                        'dired-video-thumbnail-file)
                           (point-max)))))))))

(defun dired-video-thumbnail--display-buffer (videos source-dir dired-buf)
  "Display VIDEOS in a thumbnail buffer.
SOURCE-DIR is the original dired directory.
DIRED-BUF is the associated dired buffer."
  (let ((buf (get-buffer-create "*Video Thumbnails*"))
        (col 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Set mode first, then set buffer-local variables after
        (dired-video-thumbnail-mode)
        (setq dired-video-thumbnail--current-videos videos)
        (setq dired-video-thumbnail--source-dir source-dir)
        (setq dired-video-thumbnail--dired-buffer dired-buf)
        (insert (propertize (format "Video Thumbnails: %s (%d videos)\n"
                                    (abbreviate-file-name source-dir)
                                    (length videos))
                            'face 'header-line))
        (insert (propertize "RET: play | m: mark | u: unmark | U: unmark all | t: toggle all | q: quit\n\n"
                            'face 'shadow))
        (dolist (video videos)
          (let* ((cached (dired-video-thumbnail--cached-p video))
                 (marked (dired-video-thumbnail--file-marked-p video)))
            (dired-video-thumbnail--insert-thumbnail
             video
             (when cached (dired-video-thumbnail--cache-path video))
             marked))
          (setq col (1+ col))
          (if (>= col dired-video-thumbnail-columns)
              (progn
                (insert "\n")
                (setq col 0))
            (insert " ")))
        (goto-char (point-min))))
    (display-buffer buf)
    buf))

;;; Async generation queue

(defun dired-video-thumbnail--process-queue ()
  "Process the next video in the generation queue."
  (when (and dired-video-thumbnail--pending
             (< (length dired-video-thumbnail--processes) 4))
    (let ((video (pop dired-video-thumbnail--pending)))
      (unless (dired-video-thumbnail--cached-p video)
        (message "Generating thumbnail for %s... (%d remaining)"
                 (file-name-nondirectory video)
                 (length dired-video-thumbnail--pending))
        (dired-video-thumbnail--generate-async
         video
         (lambda (thumb-path)
           (dired-video-thumbnail--update-thumbnail
            video
            (if thumb-path thumb-path :error))
           (dired-video-thumbnail--process-queue))))
      (dired-video-thumbnail--process-queue))))

(defun dired-video-thumbnail--generate-missing (videos)
  "Queue generation of missing thumbnails for VIDEOS."
  (let ((missing (seq-filter (lambda (v)
                               (not (dired-video-thumbnail--cached-p v)))
                             videos)))
    (when missing
      (setq dired-video-thumbnail--pending
            (append dired-video-thumbnail--pending missing))
      (dired-video-thumbnail--process-queue))))

;;; Interactive commands

;;;###autoload
(defun dired-video-thumbnail ()
  "Display thumbnails for video files in current dired buffer.
If files are marked, show thumbnails for marked videos only.
Otherwise, show thumbnails for all videos in the directory."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (let* ((dired-buf (current-buffer))
         (has-marks (save-excursion
                      (goto-char (point-min))
                      (re-search-forward dired-re-mark nil t)))
         (videos (if has-marks
                     (dired-get-marked-files nil nil #'dired-video-thumbnail--video-p)
                   (seq-filter #'dired-video-thumbnail--video-p
                               (directory-files default-directory t nil t))))
         (source-dir default-directory))
    (unless videos
      (user-error "No video files found"))
    (message "Found %d video files" (length videos))
    (dired-video-thumbnail--display-buffer videos source-dir dired-buf)
    (dired-video-thumbnail--generate-missing videos)))

(defun dired-video-thumbnail-play ()
  "Play the video at point."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (if dired-video-thumbnail-video-player
          (start-process "video-player" nil
                         dired-video-thumbnail-video-player video)
        (browse-url-xdg-open video))
    (user-error "No video at point")))

(defun dired-video-thumbnail-regenerate ()
  "Regenerate the thumbnail for video at point."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (let ((cache-path (dired-video-thumbnail--cache-path video)))
        (when (file-exists-p cache-path)
          (delete-file cache-path))
        (message "Regenerating thumbnail for %s..." (file-name-nondirectory video))
        (dired-video-thumbnail--generate-async
         video
         (lambda (thumb-path)
           (dired-video-thumbnail--update-thumbnail video thumb-path)
           (message "Thumbnail regenerated"))))
    (user-error "No video at point")))

(defun dired-video-thumbnail-regenerate-all ()
  "Regenerate all thumbnails in the buffer."
  (interactive)
  (when dired-video-thumbnail--current-videos
    (dolist (video dired-video-thumbnail--current-videos)
      (let ((cache-path (dired-video-thumbnail--cache-path video)))
        (when (file-exists-p cache-path)
          (delete-file cache-path))))
    (dired-video-thumbnail--generate-missing dired-video-thumbnail--current-videos)))

(defun dired-video-thumbnail-next ()
  "Move to the next video thumbnail."
  (interactive)
  (let ((pos (next-single-property-change (point) 'dired-video-thumbnail-file)))
    (when pos
      (goto-char pos)
      (unless (get-text-property (point) 'dired-video-thumbnail-file)
        (goto-char (or (next-single-property-change (point)
                                                    'dired-video-thumbnail-file)
                       (point)))))))

(defun dired-video-thumbnail-previous ()
  "Move to the previous video thumbnail."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'dired-video-thumbnail-file)))
    (when pos
      (goto-char pos)
      (let ((start (previous-single-property-change (point)
                                                    'dired-video-thumbnail-file)))
        (when start
          (goto-char start))))))

(defun dired-video-thumbnail-increase-size ()
  "Increase thumbnail display size."
  (interactive)
  (setq dired-video-thumbnail-display-height
        (+ (or dired-video-thumbnail-display-height 150) 25))
  (dired-video-thumbnail-refresh))

(defun dired-video-thumbnail-decrease-size ()
  "Decrease thumbnail display size."
  (interactive)
  (setq dired-video-thumbnail-display-height
        (max 50 (- (or dired-video-thumbnail-display-height 150) 25)))
  (dired-video-thumbnail-refresh))

(defun dired-video-thumbnail-refresh ()
  "Refresh the thumbnail display."
  (interactive)
  (when (and dired-video-thumbnail--current-videos
             dired-video-thumbnail--source-dir
             dired-video-thumbnail--dired-buffer)
    (let ((current-file (get-text-property (point) 'dired-video-thumbnail-file)))
      (dired-video-thumbnail--display-buffer
       dired-video-thumbnail--current-videos
       dired-video-thumbnail--source-dir
       dired-video-thumbnail--dired-buffer)
      ;; Restore position to the same file
      (when current-file
        (dired-video-thumbnail--goto-file current-file)))))

(defun dired-video-thumbnail--goto-file (file)
  "Move point to the thumbnail for FILE."
  (goto-char (point-min))
  (let ((found nil))
    (while (and (not found) (not (eobp)))
      (if (equal file (get-text-property (point) 'dired-video-thumbnail-file))
          (setq found t)
        (goto-char (or (next-single-property-change (point) 'dired-video-thumbnail-file)
                       (point-max)))))
    found))

(defun dired-video-thumbnail-clear-cache ()
  "Clear all cached thumbnails."
  (interactive)
  (when (yes-or-no-p "Clear all cached video thumbnails? ")
    (when (file-directory-p dired-video-thumbnail-cache-dir)
      (delete-directory dired-video-thumbnail-cache-dir t))
    (message "Thumbnail cache cleared")))

;;; Marking commands

(defun dired-video-thumbnail--count-marked ()
  "Count marked files in the associated dired buffer."
  (let ((count 0))
    (dolist (video dired-video-thumbnail--current-videos)
      (when (dired-video-thumbnail--file-marked-p video)
        (setq count (1+ count))))
    count))

(defun dired-video-thumbnail-mark ()
  "Mark the video at point in both thumbnail and dired buffers."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (let ((count nil))
        (dired-video-thumbnail--mark-in-dired video ?*)
        (dired-video-thumbnail-refresh)
        (setq count (dired-video-thumbnail--count-marked))
        (dired-video-thumbnail-next)
        (message "Marked: %s (%d total)"
                 (file-name-nondirectory video)
                 count))
    (user-error "No video at point")))

(defun dired-video-thumbnail-unmark ()
  "Unmark the video at point in both thumbnail and dired buffers."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (let ((count nil))
        (dired-video-thumbnail--mark-in-dired video ?\s)
        (dired-video-thumbnail-refresh)
        (setq count (dired-video-thumbnail--count-marked))
        (dired-video-thumbnail-next)
        (message "Unmarked: %s (%d marked)"
                 (file-name-nondirectory video)
                 count))
    (user-error "No video at point")))

(defun dired-video-thumbnail-toggle-mark ()
  "Toggle mark on the video at point."
  (interactive)
  (if-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
      (if (dired-video-thumbnail--file-marked-p video)
          (dired-video-thumbnail-unmark)
        (dired-video-thumbnail-mark))
    (user-error "No video at point")))

(defun dired-video-thumbnail-unmark-all ()
  "Unmark all videos in both thumbnail and dired buffers."
  (interactive)
  (dolist (video dired-video-thumbnail--current-videos)
    (dired-video-thumbnail--mark-in-dired video ? ))
  (dired-video-thumbnail-refresh)
  (message "All marks removed"))

(defun dired-video-thumbnail-mark-all ()
  "Mark all videos in both thumbnail and dired buffers."
  (interactive)
  (dolist (video dired-video-thumbnail--current-videos)
    (dired-video-thumbnail--mark-in-dired video ?*))
  (dired-video-thumbnail-refresh)
  (message "Marked all %d videos" (length dired-video-thumbnail--current-videos)))

(defun dired-video-thumbnail-toggle-all-marks ()
  "Toggle marks on all videos."
  (interactive)
  (dolist (video dired-video-thumbnail--current-videos)
    (if (dired-video-thumbnail--file-marked-p video)
        (dired-video-thumbnail--mark-in-dired video ? )
      (dired-video-thumbnail--mark-in-dired video ?*)))
  (dired-video-thumbnail-refresh)
  (message "%d videos now marked" (dired-video-thumbnail--count-marked)))

(defun dired-video-thumbnail-goto-dired ()
  "Switch to the associated dired buffer."
  (interactive)
  (if (and dired-video-thumbnail--dired-buffer
           (buffer-live-p dired-video-thumbnail--dired-buffer))
      (pop-to-buffer dired-video-thumbnail--dired-buffer)
    (when dired-video-thumbnail--source-dir
      (dired dired-video-thumbnail--source-dir))))

(defun dired-video-thumbnail-get-marked ()
  "Return list of marked videos, or video at point if none marked."
  (let ((marked (seq-filter #'dired-video-thumbnail--file-marked-p
                            dired-video-thumbnail--current-videos)))
    (or marked
        (when-let ((video (get-text-property (point) 'dired-video-thumbnail-file)))
          (list video)))))

(defun dired-video-thumbnail-delete-marked ()
  "Delete marked videos (or video at point if none marked)."
  (interactive)
  (let ((files (dired-video-thumbnail-get-marked)))
    (unless files
      (user-error "No videos to delete"))
    (when (yes-or-no-p (format "Delete %d video(s)? " (length files)))
      (dolist (file files)
        (delete-file file t)
        (setq dired-video-thumbnail--current-videos
              (delete file dired-video-thumbnail--current-videos)))
      ;; Refresh dired buffer
      (when (and dired-video-thumbnail--dired-buffer
                 (buffer-live-p dired-video-thumbnail--dired-buffer))
        (with-current-buffer dired-video-thumbnail--dired-buffer
          (revert-buffer)))
      (dired-video-thumbnail-refresh)
      (message "Deleted %d video(s)" (length files)))))

;;; Keymaps

(defvar dired-video-thumbnail-item-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'dired-video-thumbnail-play)
    (define-key map [mouse-3] #'dired-video-thumbnail-toggle-mark)
    (define-key map [return] #'dired-video-thumbnail-play)
    map)
  "Keymap for individual thumbnail items.")

(defvar dired-video-thumbnail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'dired-video-thumbnail-play)
    (define-key map (kbd "g") #'dired-video-thumbnail-regenerate)
    (define-key map (kbd "G") #'dired-video-thumbnail-regenerate-all)
    (define-key map (kbd "d") #'dired-video-thumbnail-goto-dired)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "n") #'dired-video-thumbnail-next)
    (define-key map (kbd "p") #'dired-video-thumbnail-previous)
    (define-key map (kbd "+") #'dired-video-thumbnail-increase-size)
    (define-key map (kbd "-") #'dired-video-thumbnail-decrease-size)
    (define-key map (kbd "r") #'dired-video-thumbnail-refresh)
    ;; Marking commands
    (define-key map (kbd "m") #'dired-video-thumbnail-mark)
    (define-key map (kbd "u") #'dired-video-thumbnail-unmark)
    (define-key map (kbd "SPC") #'dired-video-thumbnail-toggle-mark)
    (define-key map (kbd "U") #'dired-video-thumbnail-unmark-all)
    (define-key map (kbd "M") #'dired-video-thumbnail-mark-all)
    (define-key map (kbd "x") #'dired-video-thumbnail-delete-marked)
    (define-key map (kbd "t") #'dired-video-thumbnail-toggle-all-marks)
    map)
  "Keymap for `dired-video-thumbnail-mode'.")

;;; Major mode

(define-derived-mode dired-video-thumbnail-mode special-mode "Video-Thumbs"
  "Major mode for viewing video thumbnails.

\\{dired-video-thumbnail-mode-map}"
  :group 'dired-video-thumbnail
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;; Cleanup

(defun dired-video-thumbnail--cleanup ()
  "Clean up any running processes."
  (dolist (proc dired-video-thumbnail--processes)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq dired-video-thumbnail--processes nil)
  (setq dired-video-thumbnail--pending nil))

(add-hook 'kill-emacs-hook #'dired-video-thumbnail--cleanup)

(provide 'dired-video-thumbnail)
;;; dired-video-thumbnail.el ends here
