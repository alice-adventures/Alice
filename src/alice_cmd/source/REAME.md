# Command Group `source`

  1. `source [--list] | [--init | --status | --update] <psource>`
     * `--list` : list all Problem Sources currently available

     * `--init <psource>` : init `<psource>` for participation:

       * clone the repository `alice-adventures/<psource>` into the directory
         `alice/<psource>`
       * run `alr update` in `alice/<psource>`
         * clone the repository `alice-adventures/<psource>-shared` in the
           directory `alice/<psource>/shared`
       * run `alr build` in `alice/<psource>`
       * creates the repository `<user>/alice-<psource>` in GitHub for the
       user, if it does not exists, based on the
       `alice-adventures/<psource>-template` repository
       * clone the repository `<user>/alice-<psource>` in the directory
         `alice/<psource>/usr/<user>`

     * `--update <psource>` :
       * update Problem Source `<psource>` (`alr update` & `alr build`)

     * `--status <psource>` : show overall status of `<psource>`


  2. `share [--check | --get | --post] <psource>`
      * `--check` : check if there are new shared assets
      * `--get` : get new assets added into the shared repository
      * `--pots` : post new assets created by current user


  3. `solutions [--check | --get <user> | --post <ref>] <psource>`
     * `--check` : check if there are new solutions published by other
       participants
     * `--get <user>` : get new solutions published by participant `<user>`
       (`<user>` can be `--all` to get solutions from all participants; in
       the future it could support a comma-separated list of users)
     * `--post` : post solution of problem `<ref>` so that other participants
       can review it


   4. `render [--add-gui] <psource> <ref>` : Create the required files for
      problem `<ref>`, possibly adding support for GUI
      * (optional) check if it really exists the problem `<ref>` in the web
         of `<psorce>`
      * check if there are assets in the `<psource>/share` directory for the
        problem `<ref>`
        * use them when present
        * otherwise, warn the user and proceed: this will generate a
          (partial) set of files that might lack descriptions or input files;
          also, encourage the user to create and share such resources


  1. `update <psource>` : updates all `<psource>` stuff, including
     * main repository
     * template repository
     * shared resources

  1. `push <psource>` : push new problems solved into user repository

  1. `generate <psource> [--gui]` : generates stub of new shared problems,
     possibly including GUI interface

  1. `build `<psource>` [--usr USER | --all]` : builds all `<psource>` problems for
     user, for the given user, or for all users.
