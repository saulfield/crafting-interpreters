from setuptools import find_packages, setup

setup(
    name="plox",
    version="0.0.1",
    description="Lox interpreter",
    url="https://craftinginterpreters.com",
    packages=find_packages(exclude=["tests"]),
    python_requires=">=3.10",
    install_requires=[],
    entry_points={
        "console_scripts": [
            "lox = plox.main:main",
        ]
    },
)
