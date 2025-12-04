# Makefile for dired-video-thumbnail documentation

# File names
TEXI_FILE = dired-video-thumbnail.texi
INFO_FILE = dired-video-thumbnail.info
HTML_FILE = dired-video-thumbnail.html
PDF_FILE = dired-video-thumbnail.pdf

# Set default values if not specified
PREFIX ?= /usr
infodir ?= $(PREFIX)/share/info

# Tools
MAKEINFO = makeinfo
TEXI2PDF = texi2pdf
TEXI2HTML = makeinfo --html --no-split
INSTALL_INFO = install-info

.PHONY: all info html pdf clean install install-docs docs help

# Default target
all: info html

# Alias for building info
docs: info

# Build Info file (for Emacs)
info: $(INFO_FILE)

$(INFO_FILE): $(TEXI_FILE)
	$(MAKEINFO) --no-split $< -o $@

# Build HTML file
html: $(HTML_FILE)

$(HTML_FILE): $(TEXI_FILE)
	$(TEXI2HTML) $< -o $@

# Build PDF file
pdf: $(PDF_FILE)

$(PDF_FILE): $(TEXI_FILE)
	$(TEXI2PDF) $<

# Install Info file to Emacs info directory
# Usage: make install [infodir=/path/to/info] [PREFIX=/usr]
# Examples:
#   make install                          # Install to /usr/share/info
#   make install PREFIX=/usr/local        # Install to /usr/local/share/info
#   make install infodir=~/.emacs.d/info  # Install to personal info directory
install: install-docs

install-docs: docs
	@echo "Installing info documentation to $(DESTDIR)$(infodir)..."
	install -d $(DESTDIR)$(infodir)
	install -m 644 $(INFO_FILE) $(DESTDIR)$(infodir)/
	$(INSTALL_INFO) --info-dir=$(DESTDIR)$(infodir) $(DESTDIR)$(infodir)/$(INFO_FILE)
	@echo "Info file installed successfully!"
	@echo "Access in Emacs with: C-h i d m dired-video-thumbnail RET"

# Clean generated files
clean:
	rm -f $(INFO_FILE) $(HTML_FILE) $(PDF_FILE)
	rm -f *.aux *.cp *.fn *.ky *.log *.pg *.toc *.tp *.vr
	rm -f *.cps *.fns *.vrs

# Help target
help:
	@echo "Available targets:"
	@echo "  make info     - Build Info file for Emacs (default)"
	@echo "  make docs     - Alias for 'make info'"
	@echo "  make html     - Build HTML documentation"
	@echo "  make pdf      - Build PDF documentation"
	@echo "  make all      - Build Info and HTML files"
	@echo "  make install  - Install Info file with automatic registration"
	@echo "  make clean    - Remove generated files"
	@echo ""
	@echo "Installation examples:"
	@echo "  make install                          # Install to /usr/share/info (may need sudo)"
	@echo "  make install PREFIX=/usr/local        # Install to /usr/local/share/info"
	@echo "  make install infodir=~/.emacs.d/info  # Install to personal info directory"
	@echo ""
	@echo "Variables:"
	@echo "  PREFIX  - Installation prefix (default: /usr)"
	@echo "  infodir - Info directory (default: \$${PREFIX}/share/info)"
	@echo "  DESTDIR - Staging directory for package builds (default: empty)"
