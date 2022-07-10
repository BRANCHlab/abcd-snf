# Clustering and Classification for Paediatric mTBI

This repository contains a [quarto](https://quarto.org/) book containing all the documentation associated with my PhD research projects.

The quarto book structure allows me to stitch scripts (symbolically linked to the [wheeler-lab github](https://github.com/eman-nishat/wheeler-lab)), code (from the wheeler-lab carbon drive), and prose into a wide range of output formats.

To read the book, you can either open a pre-rendered version (the .pdf file or one of the .html files in /\_book\/, soon to be uploaded to carbon), or render the book yourself:

1. Install quarto https://quarto.org/docs/get-started/
2. Establish a symbolic link to `lab-data` (from our lab carbon folder) in the `research/` directory
3. Establish a symbolic link to `subtyping-mtbi` (from the [subtyping-mtbi repository](https://github.com/psvelayudhan/subtyping-mtbi)) in the `research/` directory
4. `cd` to this project folder and run the command `quarto render .`
