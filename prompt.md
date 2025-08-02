build a comprehensive vanilla emacs 30 configuration using the built-in package manager. it must disable native compile warnings from showing. it must have a minimal ui. for fonts, setup default, fixed-fonts, variable-pitch and unicode fonts using jetbrainsmono nerd font. also add the following configurations for emacs packages for enhanced ui: catppuccin macchiato theme, doom modeline, solaire-mode, nerd-icons, which-key, dashboard. then build evil mode into the configuration along with evil-collection and various other useful evil emacs packages. then build a completion system into vanilla emacs 30 that includes the following packages: orderless, vertico, corfu, nerd-icons-corfu, cape, marginalia, nerd-icons-completion, consult, embark and embark-consult. then build dired and dirvish into vanilla emacs 30 with nerd-icons support. then make dirvish look and feel more like the yazi file manager. then make dirvish look and feel more like the yazi file manager. then integrate org-mode into the existing configuration with sane defaults and settings. this org configuration should be suitable for general use.then implement a robust buffer system with the following properties: make sure ibuffer looks and feels like bufler, doom-emacs like keybindings for managing buffer and nerd icons integrations. for the buffer setup, remove any buffer that starts or ends with.  make sure to search the web before writing anything and use best practices for emacs setup. use the use-package format to setup everything. do not introduce errors. do not introduce redundant configuration options. 



To the above vanilla emacs 30 configuration, implement a robust buffer system with the following properties: make sure ibuffer looks and feels like bufler, doom-emacs like keybindings for managing buffer. Then integrate this buffer setup into the existing configuration with sane defaults and settings. Make sure to search the web before writing anything and use best practices for emacs setup. Use the use-package format to setup everything. Do not introduce errors. Do not introduce redundant configuration options. Then write the changes that to be made.

Based on your current configuration and common Emacs setups, here's a refined list of packages that would benefit from :demand t:

    evil: Essential for Vim keybindings from startup.

    evil-collection: Depends on evil being loaded and sets up integrations.

    evil-surround: Sets up a global mode.

    evil-commentary: Sets up a global mode.

    evil-args: Already has :demand t, which is good.

    vertico: Core completion UI, needed early by marginalia, nerd-icons-completion.

    orderless: Sets completion-styles early.

    marginalia: Depends on vertico.

    corfu: Core completion-at-point, sets up global-corfu-mode.

    nerd-icons-corfu: Already has :demand t, which is good.

    nerd-icons-completion: Already has :demand t, which is good.

    cape: Adds functions to completion-at-point-functions, which should be available early.

    consult: Binds many commands, useful to be available immediately.

    embark: Binds universal action keys.

    embark-consult: Depends on embark and consult.

    dirvish: You want it to take over Dired quickly and smoothly.

    nerd-icons: While not strictly :demand t for its functions, :custom settings for font family and color icons should be applied early. It's often good practice to :demand t for core UI packages if their custom settings impact other packages that load earlier.

    doom-modeline: You want the modeline to be styled correctly from the start.

    solaire-mode: Applies visual distinctions early.

    which-key: You want key hints to appear immediately.

    dashboard: You want the dashboard to appear on startup.
