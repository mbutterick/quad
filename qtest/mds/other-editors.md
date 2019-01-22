# Command-Line Tools and Your Editor of Choice

Although DrRacket is the easiest way for most people to start with
Racket, many Racketeers prefer command-line tools and other text
editors.  The Racket distribution includes several command-line tools,
and popular editors include or support packages to make them work well
with Racket.

    1 Command-Line Tools                       
      1.1 Compilation and Configuration: `raco`
      1.2 Interactive evaluation               
      1.3 Shell completion                     
                                               
    2 Emacs                                    
      2.1 Major Modes                          
      2.2 Minor Modes                          
      2.3 Packages specific to Evil Mode       
                                               
    3 Vim                                      
                                               
    4 Sublime Text                             

## 1. Command-Line Tools

Racket provides, as part of its standard distribution, a number of
command-line tools that can make racketeering more pleasant.

### 1.1. Compilation and Configuration: `raco`

The `raco` \(short for “**Ra**cket **co**mmand”\) program provides a
command-line interface to many additional tools for compiling Racket
programs and maintaining a Racket installation.

* `raco make` compiles Racket source to bytecode.

  For example, if you have a program `"take-over-world.rkt"` and you’d
  like to compile it to bytecode, along with all of its dependencies, so
  that it loads more quickly, then run

    `raco make take-over-the-world.rkt`

  The bytecode file is written as `"take-over-the-world_rkt.zo"` in a
  `"compiled"` subdirectory; `".zo"` is the file suffix for a bytecode
  file.

* `raco setup` manages a Racket installation, including manually
  installed packages.

  For example, if you create your own library collection called
  `"take-over"`, and you’d like to build all bytecode and documentation
  for the collection, then run

    `raco setup take-over`

* `raco pkg` manages packages that can be installed through the Racket
  package manager.

  For example, to see the list of installed packages run:

    `raco pkg show`

  To install a new package named `<package-name>` run:

    `raco pkg install <package-name>`

  See \[missing\] for more details about package management.

For more information on `raco`, see \[missing\].

### 1.2. Interactive evaluation

The Racket REPL provides everything you expect from a modern interactive
environment. For example, it provides an `,enter` command to have a REPL
that runs in the context of a given module, and an `,edit` command to
invoke your editor \(as specified by the `EDITOR` environment variable\)
on the file you entered. A `,drracket` command makes it easy to use your
favorite editor to write code, and still have DrRacket at hand to try
things out.

For more information, see \[missing\].

### 1.3. Shell completion

Shell auto-completion for `bash` and `zsh` is available in
`"share/pkgs/shell-completion/racket-completion.bash"` and
`"share/pkgs/shell-completion/racket-completion.zsh"`, respectively. To
enable it, just run the appropriate file from your `.bashrc` or your
`.zshrc`.

The `"shell-completion"` collection is only available in the Racket Full
distribution. The completion scripts are also available
[online](https://github.com/racket/shell-completion).

## 2. Emacs

Emacs has long been a favorite among Lispers and Schemers, and is
popular among Racketeers as well.

### 2.1. Major Modes

* [Racket mode](https://github.com/greghendershott/racket-mode) provides
  thorough syntax highlighting and DrRacket-style REPL and buffer
  execution support for Emacs.

  Racket mode can be installed via [MELPA](http://melpa.milkbox.net) or
  manually from the Github repository.

* [Quack](http://www.neilvandyke.org/quack/) is an extension of Emacs’s
  `scheme-mode` that provides enhanced support for Racket, including
  highlighting and indentation of Racket-specific forms, and
  documentation integration.

  Quack is included in the Debian and Ubuntu repositories as part of the
  `emacs-goodies-el` package. A Gentoo port is also available \(under
  the name `app-emacs/quack`\).

* [Geiser](http://www.nongnu.org/geiser/) provides a programming
  environment where the editor is tightly integrated with the Racket
  REPL. Programmers accustomed to environments such as Slime or Squeak
  should feel at home using Geiser. Geiser requires GNU Emacs 23.2 or
  better.

  Quack and Geiser can be used together, and complement each other
  nicely. More information is available in the [Geiser
  manual](http://www.nongnu.org/geiser/).

  Debian and Ubuntu packages for Geiser are available under the name
  `geiser`.

* Emacs ships with a major mode for Scheme, `scheme-mode`, that while
  not as featureful as the above options, works reasonably well for
  editing Racket code. However, this mode does not provide support for
  Racket-specific forms.

* No Racket program is complete without documentation. Scribble support
  for Emacs is available with Neil Van Dyke’s [Scribble
  Mode](http://www.neilvandyke.org/scribble-emacs/).

  In addition, `texinfo-mode` \(included with GNU Emacs\) and  plain
  text modes work well when editing Scribble  documents. The Racket
  major modes above are not really suited  to this task, given how
  different Scribble’s syntax is from  Racket’s.

### 2.2. Minor Modes

* [Paredit](http://mumble.net/~campbell/emacs/paredit.el) is a minor
  mode for pseudo-structurally editing programs in Lisp-like languages.
  In addition to providing high-level S-expression editing commands, it
  prevents you from accidentally unbalancing parentheses.

  Debian and Ubuntu packages for Paredit are available under the name
  `paredit-el`.

* [Smartparens](https://github.com/Fuco1/smartparens) is a minor mode
  for editing s-expressions, keeping parentheses balanced, etc.  Similar
  to Paredit.

* Alex Shinn’s
  [scheme-complete](http://synthcode.com/wiki/scheme-complete) provides
  intelligent, context-sensitive code completion. It also integrates
  with Emacs’s `eldoc` mode to provide live documentation in the
  minibuffer.

  While this mode was designed for R5RS, it can still be useful for
  Racket development. The tool is unaware of large portions of the
  Racket standard library, and there may be some discrepancies in the
  live documentation in cases where Scheme and Racket have diverged.

* The
  [RainbowDelimiters](http://www.emacswiki.org/emacs/RainbowDelimiters)
  mode colors parentheses and other delimiters according to their
  nesting depth. Coloring by nesting depth makes it easier to know, at a
  glance, which parentheses match.

* [ParenFace](http://www.emacswiki.org/emacs/ParenFace) lets you choose
  in which face \(font, color, etc.\) parentheses should be displayed.
  Choosing an alternate face makes it possible to make “tone down”
  parentheses.

### 2.3. Packages specific to Evil Mode

* [on-parens](https://github.com/willghatch/emacs-on-parens) is a
  wrapper for smartparens motions to work better with evil-mode’s normal
  state.

* [evil-surround](https://github.com/timcharper/evil-surround) provides
  commands to add, remove, and change parentheses and other delimiters.

* [evil-textobj-anyblock](https://github.com/noctuid/evil-textobj-anyblock)
  adds a text-object that matches the closest of any parenthesis or
  other delimiter pair.

## 3. Vim

Many distributions of Vim ship with support for Scheme, which will
mostly work for Racket. You can enable filetype detection of Racket
files as Scheme with the following:

  `if has("autocmd")`                                
    `au BufReadPost *.rkt,*.rktl set filetype=scheme`
  `endif`                                            

Alternatively, you can use the
[vim-racket](https://github.com/wlangstroth/vim-racket) plugin to enable
auto-detection, indentation, and syntax highlighting specifically for
Racket files. Using the plugin is the easiest method, but if you would
like to roll your own settings or override settings from the plugin, add
something like the following to your `".vimrc"` file:

  `if has("autocmd")`                                
    `au BufReadPost *.rkt,*.rktl set filetype=racket`
    `au filetype racket set lisp`                    
    `au filetype racket set autoindent`              
  `endif`                                            

However, if you take this path you may need to do more work when
installing plugins because many Lisp-related plugins and scripts for vim
are not aware of Racket. You can also set these conditional commands in
a `"scheme.vim"` or `"racket.vim"` file in the `"ftplugin"` subdirectory
of your vim folder.

Most installations of vim will automatically have useful defaults
enabled, but if your installation does not, you will want to set at
least the following in your `".vimrc"` file:

  `" Syntax highlighting`                      
  `syntax on`                                  
                                               
  `" These lines make vim load various plugins`
  `filetype on`                                
  `filetype indent on`                         
  `filetype plugin on`                         
                                               
  `" No tabs!`                                 
  `set expandtab`                              

Indentation

You can enable indentation for Racket by setting both the `lisp` and
`autoindent` options in Vim. However, the indentation is limited and not
as complete as what you can get in Emacs. You can also use Dorai
Sitaram’s [scmindent](https://github.com/ds26gte/scmindent) for better
indentation of Racket code. The instructions on how to use the indenter
are available on the website.

If you use the built-in indenter, you can customize it by setting how to
indent certain keywords. The vim-racket plugin mentioned above sets some
default keywords for you. You can add keywords yourself in your
`".vimrc"` file like this:

  `" By default vim will indent arguments after the function name`               
  `" but sometimes you want to only indent by 2 spaces similar to`               
  `" how DrRacket indents define. Set the `lispwords' variable to`               
  `" add function names that should have this type of indenting.`                
                                                                                 
  `set                                                                           
lispwords+=public-method,override-method,private-method,syntax-case,syntax-rules`
  `set lispwords+=..more..`                                                      

Highlighting

The [Rainbow
Parenthesis](http://www.vim.org/scripts/script.php?script_id=1230)
script for vim can be useful for more visible parenthesis matching.
Syntax highlighting for Scheme is shipped with vim on many platforms,
which will work for the most part with Racket. The vim-racket script
provides good default highlighting settings for you.

Structured Editing

The [Slimv](http://www.vim.org/scripts/script.php?script_id=2531) plugin
has a paredit mode that works like paredit in Emacs. However, the plugin
is not aware of Racket. You can either set vim to treat Racket as Scheme
files or you can modify the paredit script to load on `".rkt"` files.

Scribble

Vim support for writing scribble documents is provided by the
[scribble.vim](http://www.vim.org/scripts/script.php?script_id=3756)
plugin.

Miscellaneous

If you are installing many vim plugins \(not necessary specific to
Racket\), we recommend using a plugin that will make loading other
plugins easier.
[Pathogen](http://www.vim.org/scripts/script.php?script_id=2332) is one
plugin that does this; using it, you can install new plugins by
extracting them to subdirectories in the `"bundle"` folder of your Vim
installation.

## 4. Sublime Text

The [Racket package](https://sublime.wbond.net/packages/Racket) provides
support for syntax highlighting and building for Sublime Text.
