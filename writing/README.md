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
├── figures/              # Figure files (.pdf, .png)
├── tables/               # Table files (if needed)
├── bib/                  # Bibliography
│   └── references.bib
├── supp/                 # Supplementary materials
├── build/                # Build artifacts (gitignored)
└── .latexmkrc           # LaTeX compilation configuration
```

## 🚀 Quick Start

### Compile the manuscript:

**Option 1: Using pdflatex (manual)**
```bash
cd writing
pdflatex main.tex
bibtex main
pdflatex main.tex
pdflatex main.tex
```

**Option 2: Using latexmk (automatic, recommended)**
```bash
cd writing
latexmk -pdf main.tex
```

This will automatically handle all compilation steps and create `build/main.pdf`.

### Clean build artifacts:
```bash
latexmk -c
```

## 📝 Writing Workflow

### For AI-Assisted Writing:

1. **One topic per file**: Each section file focuses on a single topic
2. **TODO comments**: Use `% TODO: AI - [task]` for AI assistance
3. **Incremental edits**: Edit one section at a time for better version control
4. **Short sentences**: Keep sentences concise for easier AI parsing and collaboration

### Editing Sections:

- To edit the **introduction**: Open `sections/01_introduction.tex`
- To edit the **methods**: Open `sections/02_methods.tex`
- And so on...

Changes are automatically included when you compile `main.tex`.

## 🤝 Collaboration

### Using Git (recommended):

1. **Edit your section** in `sections/`
2. **Stage changes**: `git add sections/01_introduction.tex`
3. **Commit with message**: `git commit -m "Added X to introduction"`
4. **Push to GitHub**: `git push`
5. **Collaborator reviews** changes via pull request

### Using Overleaf:

You can sync this repository with Overleaf:
1. In Overleaf: New Project → Import from GitHub
2. Select this repository
3. Overleaf will sync with the `writing/` folder
4. Changes push back to GitHub automatically

### Track Changes:

- **Git**: Use `git diff` to see changes
- **Cursor AI Chat**: Ask AI to review changes
- **Overleaf**: Built-in track changes feature available

## 📊 Adding Figures

1. Place figure files in `figures/`
2. Use vector formats (.pdf) when possible
3. Reference in sections like:

```latex
\begin{figure}[H]
\centering
\includegraphics[width=0.8\textwidth]{figures/fig7_survival.pdf}
\caption{Your caption here}
\label{fig:survival}
\end{figure}
```

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
- Required packages: elsarticle, natbib, amsmath, graphicx, booktabs
- Optional: latexmk (for automatic compilation)

## 📧 Contact

**Corresponding Author**: Yang Liu (yang.liu@lshtm.ac.uk)

**Affiliation**: London School of Hygiene & Tropical Medicine
