## Déployer

!! fixer opam

### Sur le dépôt Opam principal

Le dernier chapitre principal (le plus haut level présent dans le fichier) du
fichier CHANGES.md doit contenir la future version de release. C' est utilisé
par *dune-release tag* pour générer le tag git.

```shell
$ dune-release lint
$ dune-release tag
$ dune-release check
$ dune-release distrib
$ dune-release publish
$ dune-release opam pkg
$ dune-release opam submit
```

