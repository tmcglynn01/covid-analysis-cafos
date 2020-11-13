#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov 11 17:05:34 2020

@author: trevor
"""

import tabula

file = '/home/trevor/cafo/data/RAW/slaughter_list.pdf'
table = tabula.read_pdf(file)