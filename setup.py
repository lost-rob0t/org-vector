from setuptools import setup, find_packages

with open("README.org", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="org_vector",
    version="0.1.0",
    author="",
    author_email="",
    description="Vector search functionality for Org-roam notes to find similar nodes",
    long_description=long_description,
    long_description_content_type="text/plain",
    url="",
    packages=find_packages(),
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
        "scikit-learn>=1.0.0",
        "orgparse>=0.3.0",
        "psycopg>=3.0.0",
        "chromadb>=0.4.0",
        "ollama>=0.1.0",
        "langchain>=0.1.0",
        "langchain-ollama>=0.1.0",
        "pandas>=1.3.0",
        "numpy>=1.20.0",
        "posthog <=6.0.0"
    ],
    extras_require={
        "dev": [
            "pytest>=6.0",
            "pytest-cov>=2.0",
            "black>=21.0",
            "flake8>=3.8",
            "mypy>=0.812",
            "ipython>=7.0",
            "pyright>=1.1.0",
        ],
        "viz": [
            "matplotlib>=3.5.0",
            "adjusttext>=0.7.0",
            "matplotlib-inline>=0.1.0",
        ],
        "postgres": [
            "pgvector>=0.1.0",
        ],
    },
    entry_points={
        "console_scripts": [
            "org_vector=org_vector.main:main",
        ],
    },
    keywords=["org-mode", "org-roam", "vector-search", "semantic-search", "knowledge-management"],
    project_urls={
        "Bug Reports": "",
        "Source": "",
    },
)
