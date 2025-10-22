# LaTeXmk configuration for the manuscript
# Automatically compile references and handle auxiliary files

# Use pdflatex
$pdf_mode = 1;

# Output directory for build artifacts
$out_dir = 'build';

# Ensure bibtex runs when needed
$bibtex_use = 2;

# Clean up temporary files
$clean_ext = 'bbl nav snm vrb synctex.gz';

# Preview continuously
$preview_continuous_mode = 0;



