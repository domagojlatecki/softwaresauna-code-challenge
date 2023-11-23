#!/usr/bin/env sh

set -e

./mill cc + coverage
open out/app/scoverage/htmlReport.dest/index.html
