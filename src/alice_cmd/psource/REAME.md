# Command Group `psource`

  1. `list` : list all currently available Problem Sources

  1. `status [PSOURCE]` : show overall status of all Problem Sources or
     detailed information of the given PSOURCE

  1. `init PSOURCE` : init new PSOURCE for participation
     * creates new repository in github for the user, based on the
       PSOURCE-template repository

  1. `update PSOURCE` : updates all PSOURCE stuff, including
     * main repository
     * template repository
     * shared resources

  1. `push PSOURCE` : push new problems solved into user repository

  1. `log PSOURCE` : check if there are new problems solved y other
     Participants.

  1. `pull PSOURCE` : get latest problems solved by other Participants.

  1. `share PSOURCE` : push new stuff added into the shared repository

  1. `generate PSOURCE [--gui]` : generates stub of new shared problems,
     possibly including GUI interface

  1. `build PSOURCE [--usr USER | --all]` : builds all PSOURCE problems for
     user, for the given user, or for all users.
