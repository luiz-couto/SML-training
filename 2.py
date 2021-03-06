#2

class Node:
    def __init__(self):
        self.n = 0
        self.e = ''

class Stack:
    def __init__(self):
        self.size = 0
        self.top = None
    
    def add(self, str):
        newNode = Node()
        newNode.e = str

        newNode.n = self.top
        self.top = newNode
        self.size += 1

    def remove(self):
        if self.size > 0:
            rmv = self.top
            self.top = rmv.n
            self.size -= 1
            return rmv.e
        
        raise Exception('Stack is empty')

    def isNotEmpty(self):
        return self.size > 0

