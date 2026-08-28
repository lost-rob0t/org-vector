#!/usr/bin/env python3
"""Offline test runner (replaces the old model-downloading test script).

Runs the unittest suite in tests/ and prints a summary.
"""
import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
sys.path.insert(0, str(Path(__file__).resolve().parent / "tests"))


def main() -> int:
    loader = unittest.TestLoader()
    suite = loader.discover(str(Path(__file__).resolve().parent / "tests"), pattern="test_*.py")
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    raise SystemExit(main())
