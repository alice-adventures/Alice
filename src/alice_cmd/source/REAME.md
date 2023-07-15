<!--REVIEW - Check & compare with the architecture diagram -->

# Command Group `source`

  1. `source [--list] | { --init | --update | --status } <source>`
     * `--list` : list all Problem Sources currently available

     * `--init <source>` : init `<source>` for participation:

       * clone the repository `alice-adventures/<source>` into the directory
         `alice/<source>`
       * run `alr update` in `alice/<source>`
         * clone the repository `alice-adventures/<source>-shared` in the
           directory `alice/<source>/shared`
       * run `alr build` in `alice/<source>`
       * creates the repository `<user>/alice-<source>` in GitHub for the
       user, if it does not exists, based on the
       `alice-adventures/<source>-template` repository
       * clone the repository `<user>/alice-<source>` in the directory
         `alice/<source>/usr/<user>`

     * `--update <source>` :
       * update Problem Source `<source>`, including
         * `alr update` & `alr build` of `alice/<source>`
         * `alr update` & `alr build` of

     * `--status <source>` : show overall status of `<source>`


  2. `share { --check | --get | --post } <source>`
      * `--check <source>` : check if there are new shared assets
      * `--get <source>` : get new assets added into the shared repository
      * `--post <source>` : post new assets created by current user


  3. `solutions { --check <source> | --get <source> <user> [<id>] | --post <source> <id> | --patch <source> <id>}`
     * `--check <source>` : check if there are new solutions published by
       other participants
     * `--get <source> <user>` : get new solutions published by participant
       `<user>` (`<user>` can be `--all` to get solutions from all
       participants; in the future it could support a comma-separated list of
       users)
     * `--post <source> <ref>` : post solution of problem `<ref>` so that
       other participants can review it


   4. `render [--add-gui] <source> <ref>` : Create the required files for
      problem `<ref>`, possibly adding support for GUI
      * (optional) check if it really exists the problem `<ref>` in the web
         of `<psorce>`
      * check if there are assets in the `<source>/share` directory for the
        problem `<ref>`
        * use them when present
        * otherwise, warn the user and proceed: this will generate a
          (partial) set of files that might lack descriptions or input files;
          also, encourage the user to create and share such resources
