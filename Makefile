# To be run before commit:
check:
	pep8 --ignore=E501 *.py
	pyflakes *.py
