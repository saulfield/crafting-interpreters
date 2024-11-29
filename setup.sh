#!/bin/bash
python3.10 -m venv .venv
. .venv/bin/activate
pip install --upgrade pip
pip install -e .
rm -rf plox.egg-info/