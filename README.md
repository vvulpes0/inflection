# Inflecting With Minimal Statistics

One of the standard tasks in natural language processing is inflection.
Given a base form (lemma) generate the form associated with some tag,
a morphological category.
For example, in English one might be given a lemma "retry"
and the tag "V;PST" (past tense verb),
and the expected output would be "retried".

This project implements this task efficiently,
using a method similar to that which one would encounter
in an introductory phonology course.
Using the data from the [2020 SIGMORPHON shared task][1],
this system attains an accuracy of above 73% on a majority of languages,
reaching a median of 80% for Indo-European languages
and of 99% on the provided Niger-Congo set.

## How to Build

Make sure to run `git submodule update --init`
in order to obtain the data in a fresh clone.

The main portion of the software is the Haskell file `cm.lhs`.
It is known to compile with GHC version 8.10.7.

* Download and install [the Haskell Platform][2]
* Run `ghc -O2 cm.lhs`.  This yields an executable `cm`.
* Run `./cm train < test > out`:
  + `train` is a path to training data
  + `test` is a path to test data
  + `out` is the desired output path

For example, the following will train on the English training data
and generate hypotheses for its test strings.

```sh
./cm Data/DEVELOPMENT-LANGUAGES/germanic/eng.trn < Data/DEVELOPMENT-LANGUAGES/germanic/eng.tst > eng.out
```

The shell script `hypothesize.sh` automates this task.

```sh
sh hypothesize.sh Data/DEVELOPMENT-LANGUAGES/germanic/eng.trn
```

The above command trains on the English training data,
generates hypotheses for the corresponding test set,
and writes output to `Hypotheses/eng.tst`.

The `quart.lhs` file is not necessary for typical use.
It is included because I wanted to generate box plots
of the results.

## License

The software implementing this task is released under the MIT license,
see the `LICENSE` file for details.
The `Data` subdirectory is a remote link;
its contents are licensed as described therein.

[1]: https://doi.org/10.18653/v1/2020.sigmorphon-1.1
[2]: https://www.haskell.org/platform/
