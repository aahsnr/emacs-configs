# Installation Instructions

1. Backup your existing Emacs configuration:
   ```sh 
   mv ~/.config/emacs ~/.config/emacs.bak
   mv ~/.emacs ~/.emacs.bak
   ```

2. Clone this configuration:
   ```sh
   git clone https://github.com/yourusername/emacs-config.git ~/.config/emacs
   ```

3. Install required system dependencies:
  
   **Gentoo**
   ```sh
   sudo emerge emacs
   ```

   **Debian/Ubuntu**
   ```sh
   sudo apt install git ripgrep fd-find build-essential libgccjit0 libjansson4 libtree-sitter0
   ```

   **Arch Linux**
   ```sh
   sudo pacman -S git ripgrep fd gcc libgccjit tree-sitter
   ``` 

4. First startup will automatically install all packages. This may take 10-20 minutes.

5. For optimal performance, compile all packages:
   ```sh
   M-x elpaca-rebuild-all
   ```
