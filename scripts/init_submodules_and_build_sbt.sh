#!/usr/bin/env bash

# exit script if any command fails
set -e
set -o pipefail

CHIPYARD_VERSION="1.9.1"
DSPTOOLS_COMMIT=5b1e733
ROCKET_COMMIT=25e2c63
FIRESIM_COMMIT=3ae68ec
ROCKET_DSP_COMMIT=fe641d1
CDE_COMMIT=384c06b

git submodule add https://github.com/ucb-bar/dsptools.git tools/dsptools
cd tools/dsptools
git checkout $DSPTOOLS_COMMIT
cd ../..
git submodule add https://github.com/ucb-bar/rocket-dsp-utils.git tools/rocket-dsp-utils
cd tools/rocket-dsp-utils
git checkout $ROCKET_DSP_COMMIT
cd ../..

git submodule add https://github.com/chipsalliance/cde.git tools/cde
cd tools/cde
git checkout $CDE_COMMIT
cd ../..

git submodule add https://github.com/chipsalliance/rocket-chip.git generators/rocket-chip
cd generators/rocket-chip
git checkout $ROCKET_COMMIT

git submodule add https://github.com/firesim/firesim.git sims/firesim
cd sims/firesim
git checkout $FIRESIM_COMMIT
cd ../..
git config --local submodule.sims/firesim.update none
git submodule update --init --recursive
git config --local --unset-all submodule.sims/firesim.update
git submodule update --init sims/firesim
cd ../..
mv build.sbt.ignore build.sbt

if [ -d project ]; then
   echo "Directory project already exists"
else
   mkdir project
fi

# add plugins
echo -e 'addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")\naddSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.21")\naddSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.5.3")\naddSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.0")\naddSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.3")' > ./project/plugins.sbt
