Hackage mirroring tool  [![Build Status](https://travis-ci.org/hvr/hackage-mirror-tool.svg?branch=master)](https://travis-ci.org/hvr/hackage-mirror-tool)
======================


This is a simple tool for mirroring to S3-compatible object stores
(e.g. Dreamhost or AWS).

See also `hackage-mirror-tool --help`.


## Example usages

### `cronjob`-based

This is a simple example for how to set up a cronjob-based mirror job,
which is triggered every 3 minutes.

Create the following `cronjob(5)` entry:


```
*/3 * * * *  ${HOME}/bin/run_mirror_job.sh
```

The `${HOME}/bin/run_mirror_job.sh` script contains:

```bash
#!/bin/bash

mkdir -p ${HOME}/workdir/logs
cd ${HOME}/workdir/

S3_ACCESS_KEY="ASJKDS..." \
S3_SECRET_KEY="asdjhakjsdhadhadjhaljkdh..." \
timeout -k5 170 ${HOME}/bin/hackage-mirror-tool +RTS -t -RTS \
  --hackage-url      http://hackage.haskell.org \
  --hackage-pkg-url  http://hackage.haskell.org/package/ \
  --s3-base-url      https://s3.amazonaws.com \
  --s3-bucket-id     my-hackage-mirror \
   &>> ${HOME}/workdir/logs/$(date -I).log
```

The `timeout -k5 170` arguments are defined that way in order to
ensure that the current job is killed before the next cronjob gets
started.
