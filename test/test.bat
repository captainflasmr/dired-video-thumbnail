@echo off
rem set INPUT=%1

rem ffmpeg -i "%INPUT%" -c:v libx264 -c:a aac output.mkv
rem ffmpeg -i "%INPUT%" -c:v libx264 -c:a aac output.avi
rem ffmpeg -i "%INPUT%" -c:v libvpx-vp9 -c:a libopus output.webm
rem ffmpeg -i "%INPUT%" -c:v libx264 -c:a aac output.mov
rem ffmpeg -i "%INPUT%" -c:v wmv2 -c:a wmav2 output.wmv
rem ffmpeg -i "%INPUT%" -c:v flv1 -c:a mp3 output.flv
rem ffmpeg -i "%INPUT%" -c:v mpeg2video -c:a mp2 output.mpg
rem ffmpeg -i "%INPUT%" -c:v libtheora -c:a libvorbis output.ogv
rem ffmpeg -i "%INPUT%" -c:v h263 -s 352x288 -c:a aac output.3gp

rem echo Done!

for %%f in (*.mp4) do ffmpeg -i "%%f" -vf "scale=iw/2:ih/2" -c:a copy "%%~nf_shrink.mp4"

rem for %%f in (*.mp4) do ffmpeg -i "%%f" -c:v libx264 -crf 28 -c:a aac -b:a 128k "%%~nf_small.mp4"

rem ffmpeg -i input.mp4 -vf "scale=iw/2:ih/2" -c:a copy output.mp4
