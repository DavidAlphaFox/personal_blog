PAOLO DONADEO PERSONAL BLOG
===========================


What is this?
-------------

This is the source repository of my personal blog, written in Objective Caml.


License
-------

This software is distributed under the 2-clause BSD license. See the file
LICENSE.


Software dependences
--------------------

The first dependency is Linux. While in principle you should be able to compile
and run under Windows and Mac OS X, I never even tried to do so, so all the
following instructions are for the Linux OS.

To compile this blog you need the following software and libraries:

* available as Debian/Ubuntu/RedHat packages:
    1. [Objective Caml](http://caml.inria.fr/ocaml/release.en.html) (>= 3.11.1)
    2. [Findlib](http://projects.camlcity.org/projects/findlib.html) (>= 1.2.4)
    3. [OCaml Batteries Included](http://batteries.forge.ocamlcore.org/) (>= 1.2.0, but probably any recent version)
    4. [Ocamlnet](http://projects.camlcity.org/projects/ocamlnet.html) (2.2.9)
    5. [ocaml-sqlite3](http://ocaml.info/home/ocaml_sources.html#ocaml-sqlite3) (>= 1.5.1)
    6. [Sexplib](http://ocaml.janestreet.com/?q=node/13) (>= 4.2.11)
    7. [pcre-ocaml](http://www.ocaml.info/home/ocaml_sources.html#pcre-ocaml) (>= 6.0.1)
    8. [Cryptokit](http://forge.ocamlcore.org/projects/cryptokit/) (>= 1.3)
    9. [libssl-ocaml](http://savonet.rastageeks.org/browser/trunk/ocaml-ssl) (>= 0.4.3)
    10. [Lwt](http://ocsigen.org/lwt/) (>= 2.1.0)

* packages you probably must compile and install by hand:
    1. [CamlTemplate](https://forge.ocamlcore.org/projects/camltemplate/) (1.0.2)
    2. [Lwt\_equeue](http://github.com/jaked/lwt-equeue). To get a copy of Lwt\_equeue:

            $ git clone git://github.com/jaked/lwt-equeue.git


Compiling
---------

With the required libraries installed, follow these steps:

1. get the source code:

        $ git clone git://github.com/pdonadeo/personal_blog.git

2. compile it:

        $ cd personal_blog/src/
        $ ocamlbuild -j 4 blog.native
        $ cp _build/blog.native blog

   "blog" is the executable you need.

3. edit **blog\_devel.conf** and modify the options to match your environments,
   expecially the paths of doc\_root, pid\_file and netplex\_config;

4. edit **netplex\_devel.conf**. This is the Netplex configuration file and
   explaining the full syntax is out of scope here. Read the
   [excellent manual](http://projects.camlcity.org/projects/dl/ocamlnet-2.2.9/doc/html-main/Netplex_intro.html)
   for details.

   However, to configure the blog, simply search for all paths in the file and
   edit them to match your directory tree.

5. run the local HTTP server, for development:

        $ ./blog --config-file `pwd`/blog_devel.conf

6. head your browser to:
   [http://localhost:5900/login](http://localhost:5900/login), you shoud see the
   login page of the administration.


Customizing the blog (DB explained)
-----------------------------------

TODO TODO TODO


Configuring Apache
------------------

TODO TODO TODO


Running the FastCGI server
--------------------------

TODO TODO TODO

