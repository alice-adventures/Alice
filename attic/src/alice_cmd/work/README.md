<!--REVIEW - Check & compare with the architecture diagram -->

# Command Group `work`

## Source
Allows the current user to manage Problem Sources in Alice, enabling
participation and updating shared resources by other participants.

##### SYNOPSYS
```
   alice source [ --list ] | { --init [ -r <repository> ] | --status | --update } <source>
```

### Source list
List all Problem Sources currently available.

##### SYNOPSIS
```
   alice source [ --list ]
```

### Source init
Initialize a Problem Source for participation in Alice.

##### SYNOPSIS
```
   alice source --init [ -r <repository> ] <source>
```

Participating in `<source>` requires a remote repository for the user to work
in `<source>`. If `<repository>` is not specified, the remote repository for
the user is `<user>/<source>`. If it already exists, it will be inspected to
verify that it was created to contain `<source>` stuff.

##### ACTIVITIES
   1. if not present, add the Alice index in Alire (done at start)
   2. if the `<repository>` option is not present, then use `<repository> := <source>`
   3. update Alire indexes
   4. check if the user owns the remote repository `<user>/<repository>`
      * if not, then create the remote repository `<user>/<repository>`
   5. check the existence of the local directory `<source>`
      * if it exists, then check that it is a clone of the remote repository
        `alice-adventures/<source>`
      * else, clone the remote repository `alice-adventures/<source>`

   4. clone source repositories:
      + if directory `alice/<source>` doesn't exist, clone repository
      `alice-adventures/<source>` in `alice/<source>`
      * if directory `alice/<source>/shared` doesn't exist, clone
        `alice-adventures/<source>-shared` in `alice/<source>/shared`

         + clone it in `alice/<source>/usr/<user>`
         + check that the repository is a valid repository for
           `<source>`
            - *TODO - Define wat it means to be a "*valid repository for
              `<source>`*":
               - it MUST have an `alire.toml` file
               - the `alire.toml` file MUST have the pin `project_euler = {
                 path='../../'}`
               - it CAN have a valid fingerprint, defined as the md5sum
                 of `alice_adventures-<user>-<source>-<repository>`, or
                 similar (nice if it can include a secret)
            - if so, ok
            - else, error
      * else
         + create the remote repository `<user>/<repository>`
         + clone it in `alice/<source>/usr/<user>`
         + perform basic initialization of the repository
           `alice/<source>/usr/<user>`; this can include
            - run `alr init --in-place --bin <user>_<source>` in that
              directory
            - run `alr with project_euler --use-path ../../`
               - create any required symlinks (e.g., `project_euler`
                 requires `input`, `css`, `html` and `js`)
            - create first example source from the directory
              `alice/<source>/share/<PATH>`
               - e.g., for `project_euler` it would be something similar
                 to `alice/project_euler/share/0001-1000/p0001_multiples_of_3_or_5/`
   6. run `alr update` in `alice/<source>`

*TODO - TO BE REVIEWED

      * run `alr build` in `alice/<source>`
      * create the repository `<user>/alice-<source>` in GitHub for the
      user, if it does not exists, based on the
      `alice-adventures/<source>-template` repository
      * clone the repository `<user>/alice-<source>` in the directory
        `alice/<source>/usr/<user>`, if it does not exists

      * `--status <source>` : show overall status of `<source>`, including,
      but not limited to:
        * users with published solutions: who, how many
        * assets shared: for which `<id>`s
        * solutions shared: which `<id>`, how many times

      * `--update <source>` :
        * update Problem Source `<source>`, including
          * run `alr update` & `alr build` in `alice/<source>`


## Share
Manage shared resources of a given Problem Source, updating stuff published by
other participants and publishing new stuff.

##### SYNOPSIS
```
   alice share { --check | --update | --publish } <source>
```

### Share check

##### SYNOPSIS
```
   alice share --check <source>
```

Check the existence of new stuff added to the shared repository of `<source>`
since the last update.

### Share update

##### SYNOPSIS
```
   alice share --update <source> [<id>]
```

Get new assets published in the shared repository of `<source>`.

### Share publish

##### SYNOPSIS
```
   alice share --publish <source> [<message>]
```

Create a pull request with new assets created by the current user for the
shared repository of `<source>`. Can include an optional `<message>` with
additional details.


## Solutions
Manage solutions in the context of a Problem Source, both updating solutions
published by other participants and publishing user' own solutions.

##### SYNOPSIS
```
  alice solutions { --check <source> | --update <source> [<user>] [<ids>] | --publish <source> <ids> | --patch source> <ids> }
```

### Solutions check

##### SYNOPSIS
```
   alice source --check <source>
```

Check if there are new solutions in a Problem Source published by other
participants.

### Solutions update

##### SYNOPSIS
```
   alice source --update <source> [--user <user> | --id <ids>]
```
> Get new solutions published by other participants, either per `<user>` or
> per `<ids>`

### Solutions publish

```
   alice solutions publish <source>
```

    * `--post <source> <id>` : post solution of problem `<id>` so that other
      participants can review it


## Render
Create all required files of a Problem Source to allow the user to work on a
particular problem.

Shared resources contain minimal information to generate, with the appropriate
template, the corresponding files for the user to work on. It can include GUI
assets.

##### SYNOPSIS
```
   alice render <source> <id>
```

##### ACTIVITIES
*TODO - REVIEW
   1. (optional) check if it really exists the problem `<id>` in the web of
      `<sorce>`

   2. check if there are assets in the `<source>/share` directory for the
      problem `<id>`
      + use them when present
      + otherwise, warn the user and proceed: this will generate a (partial)
          set of files that might lack descriptions or input files; also,
          encourage the user to create and share such resources
