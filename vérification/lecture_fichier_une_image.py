#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import numpy as np

# CHAMP A MODIFIER EN FONCTION DU NOMBRE D'ELEMENTS DES TABLEAUX INDIVIDUELS
nb_elements = 100
L=["red","green","blue","yellow","black","white","orange","purple","brown","grey"]
x = np.zeros(nb_elements)
y = np.zeros(nb_elements)
line_count = 0
i=0
with open("res.csv", "r") as my_file:
  for line in my_file:
      str = line.split()
      line_count += 1
      x[line_count-1]=str[0]
      y[line_count-1]=str[1]
      if line_count == nb_elements:
        line_count = 0
        plt.plot(x,y,"--bo",linewidth=1.0,color=L[i])
        i=i+1

plt.title("Evolution")
plt.xlim(0.,100)
plt.ylim(0.,1.)
plt.xlabel('x (m)',fontsize=16)
plt.ylabel('concentration',fontsize=16)
plt.savefig("res.png")

print("generation de 1 image finie")

