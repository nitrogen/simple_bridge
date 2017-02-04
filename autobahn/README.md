# Testing SimpleBridge with Autobahn

1. Run SimpleBridge with any of the backends (inets, cowboy, mochiweb, webmachine, yaws):

Any of the following:
```bash
make run_inets
make run_cowboy
make run_mochiweb
make run_webmachine
make run_yaws
```

2. Open another terminal while Simplebridge is running. The rest of the instructions will be done in this terminal

3. Install python-pip and python-virtualenv (skip if you've already done this once)

```bash
sudo apt-get install -y python-virtualenv python-pip
```

4. Set up Virtual Environment

```bash
virtualenv ~/wstest
```

5. Install Autobahn (skip if you've done this once)

```bash
source ~/wstest/bin/activate
pip install autobahntestsuite
```

5. Run the autobahn test suite (back in the original terminal)

```bash
wstest -m fuzzingclient -s config.json
```

6. View results in the browser of your choice:

```bash
google-chrome reports/index.html
```
