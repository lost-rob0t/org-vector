#!/usr/bin/env python
"""Thin shim: the CLI lives in org_vector.cli so the console-script entry
point and `python main.py` share one implementation."""
from org_vector.cli import run

if __name__ == "__main__":
    raise SystemExit(run())
