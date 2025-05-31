# Command Group `setup`

  1. `check` : Check the system external dependencies (OS commands required)
     and that the GitHub API and git command can operate on user repositories
     on behalf of the Participant.


  2. `config [ --show ] | --token <github_token> | --license <spdx_id> | --refresh`

     * `--show` : Show current user configuration; default command.

     * `--token` : Set login, user name and email from the given GitHub
     personal access token (and, possibly, also from git config).

     * `--license <spdx_id>` : SPDX identifier to apply when template files
       are customized for the user.

     * `--refresh` : Read information from GitHub (adn git) and update user
     configuration.
