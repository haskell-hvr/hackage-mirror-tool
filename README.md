Hackage mirroring tool  [![Build Status](https://travis-ci.org/haskell-hvr/hackage-mirror-tool.svg?branch=master)](https://travis-ci.org/haskell-hvr/hackage-mirror-tool)
======================


This is a simple tool for mirroring to S3-compatible object stores
(e.g. Dreamhost or AWS).

See also `hackage-mirror-tool --help`.


## Resource requirements

Currently, using this tool to operate a http://hackage.haskell.org
mirror has the following requirements:

 - ~1 GiB local filesystem storage (used for by local 01-index.tar cache)
 - ~10 GiB of storage in S3 bucket (at time of writing ~7.1 GiB were needed, this size increases monotonoically over time)
 - A single-threaded `hackage-mirror-tool` run needs (less than) ~256 MiB RAM; IOW, a small 512 MiB RAM VM configuration suffices.

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
timeout -k5 170 ${HOME}/bin/hackage-mirror-tool +RTS -t -A2M -M256M -RTS \
  --hackage-url      http://hackage.haskell.org \
  --hackage-pkg-url  http://hackage.haskell.org/package/ \
  --s3-base-url      https://s3.amazonaws.com \
  --s3-bucket-id     my-hackage-mirror \
   &>> ${HOME}/workdir/logs/$(date -I).log
```

The `timeout -k5 170` arguments are defined that way in order to
ensure that the current job is killed before the next cronjob gets
started.

## Sample AWS access policy

```
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "bucketlevel",
            "Effect": "Allow",
            "Action": [
                "s3:ListBucket"
            ],
            "Resource": [
                "arn:aws:s3:::hackage-mirror-tool"
            ]
        },
        {
            "Sid": "objectlevel",
            "Effect": "Allow",
            "Action": [
                "s3:GetObject",
                "s3:PutObject",
                "s3:PutObjectAcl"
            ],
            "Resource": [
                "arn:aws:s3:::hackage-mirror-tool/*"
            ]
        }
    ]
}
```
