# Summary and Final Steps
***
* **Review and Sync**: After updating your `init.el`, `packages.el`, and `config.el`, close Emacs and run `~/.config/emacs/bin/doom sync` from your terminal.

* **Install Dependencies**: Ensure `tectonic` and `texlab` are installed on your system and accessible from your shell's `PATH`.

* **Configure Zotero**: Install the Better BibTeX plugin in Zotero and set up auto-exporting of your library to the `.bib` file specified in your `config.el`.

* **Start Writing**:
    * In `.tex` or `.org` files: Use the `SPC m` leader key to access the dedicated LaTeX menu for compilation, viewing, and inserting citations.
    * **Snippets**: The new programmatic snippets are prefix-based. For example:
        * `'a` -> `\alpha`
        * `'A` -> `\Alpha`
        * `` `R` `` -> `\mathbb{R}`
        * `;>=` -> `\geq`
        * `,eq` -> `\begin{equation}...`
        * `//f` -> `\frac{...}{...}`
        * `sec` -> `\section{...}`
        * `secl` -> `\section{...} \label{...}`

This definitive configuration provides a robust, highly efficient, and enjoyable environment for all your scientific writing needs within Doom Emacs.
