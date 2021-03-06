# 8

import traceback

class FormulaError(Exception):
    pass

while True:
    try:
        inp = input()
        args = inp.split(' ')

        if (len(args) != 3):
            raise FormulaError('A entrada nao consiste de 3 elementos')

        try:
            a = float(args[0])
            b = float(args[2])
            op = args[1]
        
        except ValueError:
            raise FormulaError('O primeiro e o terceiro valor da entrada devem ser numeros')

        if op == '+':
            print(a+b)
        
        elif op == '-':
            print(a-b)

        elif op == '*':
            print(a*b)
        
        elif op == '/':
            print(a/b)
        
        else:
            raise FormulaError(op + ' nao e um operador valido')

    except FormulaError:
        traceback.print_exc()