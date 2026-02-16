#!/usr/bin/env python
"""
Utility script to load logging configuration from a file.
Usage: python load_config.py [config_file] [command...]
"""
import os
import sys
from pathlib import Path

def load_config(config_file: str = "logging.conf"):
    """Load configuration from file into environment variables."""
    config_path = Path(config_file)
    if not config_path.exists():
        print(f"Configuration file {config_file} not found", file=sys.stderr)
        return False
    
    try:
        with open(config_path) as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith('#') and '=' in line:
                    key, value = line.split('=', 1)
                    os.environ[key.strip()] = value.strip()
        return True
    except Exception as e:
        print(f"Error loading config: {e}", file=sys.stderr)
        return False

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: python load_config.py [config_file] [command...]")
        sys.exit(1)
    
    config_file = sys.argv[1]
    if load_config(config_file):
        if len(sys.argv) > 2:
            # Execute the remaining command
            import subprocess
            result = subprocess.run(sys.argv[2:])
            sys.exit(result.returncode)
        else:
            print(f"Configuration loaded from {config_file}")
    else:
        sys.exit(1)