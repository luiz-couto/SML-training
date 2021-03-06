#3

class Node:
    def __init__(self):
        self.n = 0
        self.e = ''

class Queue:
    def __init__(self):
        self.size = 0
        self.first = None
        self.last = None
    
    def add(self, str):
        newNode = Node()
        newNode.e = str

        if self.size == 0:
            self.first = newNode
        else:
            self.last.n = newNode

        self.last = newNode
        self.size += 1
    
    def remove(self):
        if self.size > 0:
            rmv = self.first
            
            if self.size > 1:
                self.first = rmv.n
            else:
                self.first = None
                self.last = None
            
            self.size -= 1

            return rmv.e
        
        raise Exception('Queue is empty')

    def isNotEmpty(self):
        return self.size > 0

    def getSmaller(self):
        curr = self.first
        smaller = self.first.e
        while (curr != self.last):
            curr = curr.n
            if curr.e < smaller:
                smaller = curr.e
        
        return smaller

