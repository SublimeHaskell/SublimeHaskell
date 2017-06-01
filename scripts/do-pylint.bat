PYTHONPATH="$HOME/Sublime:$HOME/Sublime/Data/Packages" pylint -d C0111,R0201,R0903,W0603 *.py internals/*.py hsdev/*.py > pylint.out
