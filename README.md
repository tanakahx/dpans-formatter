# dpans-formatter
Script for formatting draft proposed American National Standard (dpANS) on Common Lisp so that each section has its own contents in place.

## Environment

* SBCL 1.2.14
* Quicklisp 2015-01-28

## Usage

### Step 1. Clone this repository

```console
$ git clone https://github.com/tanakahx/dpans-formatter.git
$ cd dpans-formatter
```

### Step 2. Download dpans2texi.el to make dpANS HTML document

[dpans2texi.el](http://users-phys.au.dk/harder/dpans.html) is necesarry for converting dpANS TeX source to HTML document. First of all, download and configure it.

```console
$ wget http://users-phys.au.dk/harder/dpans2texi-1.05.tar.gz
$ tar zxvf dpans2texi-1.05.tar.gz
$ cd dpans2texi-1.05
$ ./configure
```

Run the following command to download TeX sources and convert texinfo and HTML format.

```
$ make wget
$ make
$ make html
```

`ansicl.html` is created and includes converted HTML documents.

### Step 3. Run dpans-formatter

dpans-formatter can be run from your shell directly. The argument is the *index* file of the HTML document, namely `index.html`. In case of Windows and Mac OS X, however, they does not have case-sensitivity on filename, converted `index.html` and `Index.html` is identified as the same file, so you have to *escape* `index.html` to a safe place by renaming it `ansicl.html` for example.

```console
$ mv ansicl.html/index.html ansicl.html/ansicl.html # prevent Windows and Mac OS X from identifying with Index.html
```

When you run the script, you get the following outputs.

```console
$ ./dpans-formatter.lisp ansicl.html/ansicl.html
Making a directory ansicl.html.formatted/
Copying files from ansicl.html/ to ansicl.html.formatted/
Converting Ch. Credits                    => ansicl.html.formatted/Credits.html                     (  42159 bytes)
Converting Ch. Introduction               => ansicl.html.formatted/Introduction.html                ( 143542 bytes)
Converting Ch. Syntax                     => ansicl.html.formatted/Syntax.html                      ( 139598 bytes)
Converting Ch. Evaluation-and-Compilation => ansicl.html.formatted/Evaluation-and-Compilation.html  ( 176597 bytes)
Converting Ch. Types-and-Classes          => ansicl.html.formatted/Types-and-Classes.html           (  69878 bytes)
Converting Ch. Data-and-Control-Flow      => ansicl.html.formatted/Data-and-Control-Flow.html       (  42842 bytes)
Converting Ch. Iteration                  => ansicl.html.formatted/Iteration.html                   (  80215 bytes)
Converting Ch. Objects                    => ansicl.html.formatted/Objects.html                     (  92441 bytes)
Converting Ch. Structures                 => ansicl.html.formatted/Structures.html                  (   2274 bytes)
Converting Ch. Conditions                 => ansicl.html.formatted/Conditions.html                  (  37428 bytes)
Converting Ch. Symbols                    => ansicl.html.formatted/Symbols.html                     (   4783 bytes)
Converting Ch. Packages                   => ansicl.html.formatted/Packages.html                    (  29174 bytes)
Converting Ch. Numbers                    => ansicl.html.formatted/Numbers.html                     (  43781 bytes)
Converting Ch. Characters                 => ansicl.html.formatted/Characters.html                  (  24187 bytes)
Converting Ch. Conses                     => ansicl.html.formatted/Conses.html                      (  15389 bytes)
Converting Ch. Arrays                     => ansicl.html.formatted/Arrays.html                      (  17087 bytes)
Converting Ch. Strings                    => ansicl.html.formatted/Strings.html                     (   4880 bytes)
Converting Ch. Sequences                  => ansicl.html.formatted/Sequences.html                   (  16873 bytes)
Converting Ch. Hash-Tables                => ansicl.html.formatted/Hash-Tables.html                 (  11375 bytes)
Converting Ch. Filenames                  => ansicl.html.formatted/Filenames.html                   (  40990 bytes)
Converting Ch. Files                      => ansicl.html.formatted/Files.html                       (  10724 bytes)
Converting Ch. Streams                    => ansicl.html.formatted/Streams.html                     (  27513 bytes)
Converting Ch. Printer                    => ansicl.html.formatted/Printer.html                     ( 143826 bytes)
Converting Ch. Reader                     => ansicl.html.formatted/Reader.html                      (  12201 bytes)
Converting Ch. System-Construction        => ansicl.html.formatted/System-Construction.html         (  11216 bytes)
Converting Ch. Environment                => ansicl.html.formatted/Environment.html                 (  16426 bytes)
Converting Ch. Glossary                   => ansicl.html.formatted/Glossary.html                    ( 240302 bytes)
Converting Ch. Appendix                   => ansicl.html.formatted/Appendix.html                    (   6131 bytes)
Converting Ch. Indecies                   => ansicl.html.formatted/Indecies.html                    ( 340111 bytes)
Converting Ch. Symbol-Index               => ansicl.html.formatted/Symbol-Index.html                ( 252144 bytes)
Converting Ch. List-of-Figures            => ansicl.html.formatted/List-of-Figures.html             (  20598 bytes)
Converting Ch. Table-of-Contents          => ansicl.html.formatted/Table-of-Contents.html           (  88092 bytes)
$
```

All formatted outputs are saved in `ansicl.html.formatted` directory in this example. The index of the document directory is now `ansicl.html.formatted/index.html` and we can view them with web browser.

```console
$ open ansicl.html.formatted/index.html
```
