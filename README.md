# Curry Bot

# Installation

You first need the haskell toolchain to build the program, I recommend
installing it through [GHCup](https://www.haskell.org/ghcup/).

Once that's intalled, you can run

```bash
ghcup tui
```

and install `GHC` and `stack`. When that's done, just run

```bash
stack install
```

to build and install the project.

Complete the `config.yaml` using the example as reference. The bot searches for
its config in `~/.config/curry_bot`.

```bash
mkdir ~/.config/curry_bot
cp config/config.yaml.example ~/.config/curry_bot/config.yaml
```

Alternatively, you can keep it in this folder and link it.

```bash
cp config/config.yaml.example config/config.yaml
ln config/config.yaml ~/.config/curry_bot/config.yaml
```

Then you need to have the database with its proper schema running, which can be done with the compose file.

```
docker compose up -d
psql -h 10.7.0.2 -U postgres curry_bot < sql/schema.sql
```

# References:
    - Reading YAML: https://stackoverflow.com/questions/13059806/reading-yaml-in-haskell
    - Aeson tutorial: https://www.fpcomplete.com/haskell/library/aeson/
    - Accesing configuration: https://stackoverflow.com/questions/6081075/access-the-configuration-parameters-through-a-monad
    - DBs in Haskell: https://book.realworldhaskell.org/read/using-databases.html
    - Extra users Slack: https://api.slack.com/enterprise/grid/testing#complete_the_org_setup
    - Developers Slack: https://api.slack.com/developer-program/sandboxes
