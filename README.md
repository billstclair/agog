# AGOG: A Game of Golems

AGOG is a two-player board game designed by Christopher St. Clair.

Rules are in [agog-rules.pdf](https://raw.githubusercontent.com/billstclair/agog/main/site/agog-rules.pdf).

## Elm Reactor

The non-networked simulator may be run in Elm Reactor:

```
cd .../agog
elm reactor
```

In another shell, compile the source into `site/elm.js`:

```
cd .../agog
bin/build
```

Then aim your browser at https://localhost:8000/site/index.html

