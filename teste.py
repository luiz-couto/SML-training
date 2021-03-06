class Animal:
    def __init__ ( self , name ) :
        self.name = name
    def __str__ ( self ) :
        return self.name + " is an animal "

    def eat ( self ) :
        print ( self.name + ", which is an animal , is eating .")

class Mammal ( Animal ):
    def __str__ ( self ) :
        return self.name + " is a mammal "

    def suckMilk ( self ) :
        print ( self.name + ", which is a mammal , is sucking milk .")

class Dog ( Mammal ):
    def __str__ ( self ) :
        return self.name + " is a dog "

    def bark ( self ) :
        print ( self.name + " is barking rather loudly .")

    def eat ( self ) :
        print ( self.name + " barks when it eats .")
        self.bark