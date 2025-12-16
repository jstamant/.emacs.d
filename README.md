Welcome to my Emacs configuration repo! If you're looking for any of
my other configuration files, check-out my [dotfiles repo](https://github.com/jstamant/dotfiles).

_(outdated screenshot)_

![Screenshot of Justin's Emacs window](https://raw.githubusercontent.com/jstamant/.emacs.d/master/assets/screenshot.png)

The general layout of my Emacs configuration files in the `lisp`
directory is as follows:

```
lisp/
  core-*.el
  feature-*.el
  lang-*.el
  user-*.el
```

I implemented this layout to add a bit of ordering to my config, but
without adopting a full-fledged module system as seen in Doom Emacs.
I'm trying to keep things simple and trying to avoid over-engineering
my configuration.

- `core` - Packages and their configuration that are necessary and
  likely used in the other types of modules.
- `feature` - Groups of packages and configuration that constitute a
  feature that running my Emacs configuration could do without.
- `lang` - Setup for specific programming languages.
- `user` - User configuration, like keybindings, generic lisp, and
  macros.

A note on the `user-*.el` files. They make the assumption that all the
previous files have been loaded. I'm not making my `feature` files
entirely stand-alone. The reason for this is to prevent
over-engineering: this is a private configuration - these modules
don't need to be 100% transferable to someone else's config.

`user-keybindings.el` - All my global keybindings. Example, I might
bind the prefix for `multiple-cursors` commands to `C-c m`, but the
bindings in that prefix map are done in the
`feature-multiple-cursors.el` file.

`user-lisp.el` - This is where loose lisp belongs, for the time being.
