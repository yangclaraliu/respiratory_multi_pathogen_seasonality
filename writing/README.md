# Manuscript: Respiratory Multi-Pathogen Seasonality

This directory contains the LaTeX manuscript for submission to **Epidemics** journal.

## 📁 Project Structure

```
writing/
├── main.tex              # Main manuscript file (includes all sections)
├── sections/             # Individual content sections
│   ├── 00_abstract.tex
│   ├── 01_introduction.tex
│   ├── 02_methods.tex
│   ├── 03_results.tex
│   ├── 04_discussion.tex
│   ├── 05_conclusions.tex
│   └── 06_acknowledgments.tex
├── bib/                  # Bibliography
│   ├── references.bib
│   └── ref_to_screen.bib
├── supp/                 # Supplementary materials
├── build/                # Build artifacts (PDFs tracked for sharing)
└── .latexmkrc           # LaTeX compilation configuration
```

## 🚀 Quick Start

### Compile the manuscript:

The current workflow uses `texcount` for word counting and `pdflatex` for compilation. The compilation script automatically:
1. Counts words in each section
2. Updates word counts in section headers
3. Compiles the PDF
4. Copies the final PDF to `build/main.pdf`

**To compile:**
```bash
cd writing
texcount -1 -sum -q sections/01_introduction.tex > build/introduction_wc.tmp && \
texcount -1 -sum -q sections/02_methods.tex > build/methods_wc.tmp && \
texcount -1 -sum -q sections/03_results.tex > build/results_wc.tmp && \
texcount -1 -sum -q sections/04_discussion.tex > build/discussion_wc.tmp && \
pdflatex -interaction=nonstopmode main.tex && \
cp main.pdf build/main.pdf
```

**Alternative: Using latexmk (for full builds with bibliography)**
```bash
cd writing
latexmk -pdf main.tex
```

**Note**: PDFs in `build/` are tracked in git for easy sharing with collaborators. Other build artifacts (`.aux`, `.log`, etc.) are ignored.

## 📚 Adding References

1. Edit `bib/references.bib`
2. Add BibTeX entries
3. Cite in text using `\citep{key}` or `\citet{key}`

## 🎯 Journal-Specific Notes

- **Journal**: Epidemics (Elsevier)
- **Document class**: `elsarticle`
- **Format**: Preprint, 12pt
- **Citation style**: Harvard (author-year)
- **Line numbers**: Enabled for review
- **Spacing**: Double-spaced

## 💡 Tips

1. **Compile often**: Catch errors early
2. **Use TODO comments**: Track what needs AI assistance
3. **Commit frequently**: Small commits are easier to review
4. **One idea per paragraph**: Makes editing easier
5. **Short sentences**: Better for AI parsing and collaboration

## 🔧 Requirements

- TeX Live 2024 or later
- Required packages: elsarticle, natbib, amsmath, graphicx, booktabs, siunitx, lineno
- `texcount` (for word counting - usually included with TeX Live)
- Optional: latexmk (for full builds with bibliography)

## 📧 Contact

**Corresponding Author**: Yang Liu (yang.liu@lshtm.ac.uk)

**Affiliation**: London School of Hygiene & Tropical Medicine
