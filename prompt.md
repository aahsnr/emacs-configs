Write a comprehensive completion setup for vanilla emacs 30 using corfu, cape, embark, consult, embark-consult, marginalia, vertico, and dabbrev while integrating these packages together. Use the use-package format and integrate the configuration with tokyo-night theme from doom-themes, assuming that doom-themes is already setup. Also integrate with org-src-code blocks assuming org is present in the user's configuration. Also assume package management has also been setup.  Use spacemacs keybindings under leader using general.el. Use  hex codes directly instead of using variables for the themes integration. Integrate nerd-icons into the completion system as well using nerd-icons-completion, nerd-icons-corfu. Integrate tokyo-night theme into the nerd-icons and use hex codes directly for this. Do not include project.el, projectile, ibuffer, dired, perspective integrations. Then further enhance the configuration. 


I want the following behavior from dired and dirvish from the attached configuration.
- When pressing enter on a directory, dired and dirvish should both stay on the minibuffer
- When first open dirvish using dirvish-side, dirvish shows all the properties of the directory, including the read/write permissions, user group, file/folder size. I want all these details hidden and only icons and files/folders name must be shown.
Furthermore, optimize dirvish to open faster on first start.

Add wgrep to the integration

Use the attached eglot.org file containing eglot, eldoc and flymake configurations, make sure apheleia is well integrated with eglot, eldoc, and flymake configurations. You do not need to rewrite the configuration in the attached eglot.org file. Find and remove any redundant configuration options, but make to sure to keep the configuration options that are needed.

Write a comphrensive helpful configuration for emacs 30 using https://github.com/Wilfred/helpful. Strictly setup only helpful and nothing else. You may setup anything that only complements the helpful package. Use the use-package format. And use general.el using SPC as leader for the keybindings. You may use the attached file containing the completion system for any suggestions or ideas.

Write a ibuffer configuration that looks and feels like bufler from https://github.com/alphapapa/bufler.el. The ibuffer configuration must integrate with nerd icons, gruvbox dark theme. The theme integration must use hex codes directly in custom set faces. The configuration must not use variables for these hex codes. The ibuffer configuration must also integrate with perspective.el, project.el (built-in in emacs) and treemacs. Use the use-package format to setup the configuration, and use spacemacs-like keybindings using general.el format. Assume the user has already setup configurations for nerd-icons, perspective.el, project.el, treemacs and general.el in their emacs 30 configuration. Strictly setup ibuffer for emacs 30 using these instructions.

Setup spacious-padding from https://github.com/protesilaos/spacious-padding?tab=readme-ov-file in emacs 30 using use-package format, assuming that the user has a base emacs configuration.

Write a flyspell configuration with hunspell as the backend for emacs 30, provided the user has a working base emacs configuration. Make sure flyspell is disabled for certain unnecessay parts of the config. In the org-mode, disable it in the headlines and inside the org source code blocks. Do not include any keybindings. Strictly setup flyspell and nothing else. Integrate the flyspell configuration with popup from https://github.com/auto-complete/popup-el. Use the use-package format to setup the configuration. Strictly setup flyspell and its related packages, but nothing else.

Find and fix any errors/issues in the configuration. Then remove any redundant configurations, but make sure to the configurations that are needed. Then rewrite the whole configuration. Strictly keep the configuration around setting up flyspell.


For the attached emacs configuration file, change from gruvbox dark theme to catppuccin mocha. You do not need to rewrite the whole configuration. You just need to provide the configuration for installing the catppuccin theme in use-package format. You also need to provide the sections that show the hex code changes from gruvbox dark theme to catppuccin mocha theme. You also need to provide the integration of catppuccin mocha theme with solaire-mode. Do not make any other changes in the confiugration. 
