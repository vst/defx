# Open Exchange Rates Python Client Library and CLI Application

> **NOTE:** This library (and built-in CLI application) is
> experimental. It requires refactoring and testing. Use at your own
> risk.

- Requires a valid
  [Open Exchange Rates](https://openexchangerates.org/) API Access.
- For `>=` Python version 3.6
- No dependencies, a single file.
- Can be used as a drop-in Python module for library usage or as a CLI
  application.
- [Docker](https://www.docker.com/)izer and
  [Hyper](https://hyper.sh/)active.
- Computes all crosses and reverse crosses automatically.
- Supports JSON and CSV output.
- Supports HTTP callback(s) using JSON payload.

## Usage

Get latest rates:

```
python defx.py \
       --apikey <OPEN-EXCHANGE-RATES API-KEY> \
       --base USD \
       --other EUR SGD TRY \
       --task latest \
       --format json \
       --output /tmp/output.json
```

Get historical rates:

```
python defx.py \
       --apikey <OPEN-EXCHANGE-RATES API-KEY> \
       --base USD \
       --other EUR SGD TRY \
       --task historical \
       --format json \
       --output /tmp/output.json \
       --start 2017-01-01 \
       --end   2017-01-10
```

POST payload to callback:

```
python defx.py \
       --apikey <OPEN-EXCHANGE-RATES API-KEY> \
       --base USD \
       --other EUR SGD TRY \
       --task historical \
       --format json \
       --output /dev/null \
       --start 2017-01-01 \
       --end   2017-01-10
       --callback <URL1> <URL2> <URL3-WITH-BASIC-AUTH-INFO>
```

With Docker:

```
docker run --rm vehbisinan/defx:develop --apikey <OPEN-EXCHANGE-RATES> --base USD --other EUR SGD TRY --task latest --format csv
```

## License

This piece of work is licensed under
[The 3-Clause BSD License](https://opensource.org/licenses/BSD-3-Clause).

```
Copyright 2017 Vehbi Sinan Tunalioglu <vst@vsthost.com>

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```
