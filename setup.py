from setuptools import find_packages, setup

with open("README.org", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="org_vector",
    version="0.5.0",
    author="",
    author_email="",
    description="Vector search functionality for Org-roam notes to find similar nodes",
    long_description=long_description,
    long_description_content_type="text/plain",
    url="https://github.com/lost-rob0t/org-vector",
    packages=find_packages(exclude=("tests", "rage")),
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: End Users/Desktop",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Topic :: Text Processing :: General",
        "Topic :: Scientific/Engineering :: Information Analysis",
    ],
    python_requires=">=3.8",
    install_requires=[
        "sentence-transformers>=2.2.0",
        "orgparse>=0.3.0",
        "chromadb>=0.4.0",
        "langchain>=0.1.0",
        "numpy>=1.20.0",
        "inotify-simple>=1.3.5; platform_system == 'Linux'",
        "tomli>=2.0.1; python_version < '3.11'",
    ],
    extras_require={
        "dev": [
            "pytest>=6.0",
            "black>=21.0",
            "flake8>=3.8",
            "mypy>=0.812",
            "pyright>=1.1.0",
        ],
    },
    entry_points={
        "console_scripts": [
            "org-vector=org_vector.cli:run",
        ],
    },
    keywords=["org-mode", "org-roam", "vector-search", "semantic-search", "knowledge-management"],
)
