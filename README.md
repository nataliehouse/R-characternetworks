# 📚 Character Dialogue Networks (Shiny App)

An interactive **R Shiny application** for visualising character interactions in novels as dynamic network graphs.

## ✨ Overview

This app lets you explore how fictional characters interact across chapters in a selection of well-known novels. It transforms dialogue relationships into **network graphs**, where:

- **Nodes** represent characters  
- **Edges** represent dialogue interactions between characters  
- **Edge strength** reflects number of lines spoken  
- **Chapter slider** reveals how networks evolve over time  

## 📖 Included Texts

The app currently supports:

- *The Time Traders* — Andre Norton  
- *The Mysterious Affair at Styles* — Agatha Christie  
- *The Stainless Steel Rat* — Harry Harrison  
- *A Study in Scarlet* — Arthur Conan Doyle  
- *Harry Potter and the Philosopher's Stone* — JK Rowling  
- *The Lion, The Witch and The Wardrobe* — C.S. Lewis  

## 🚀 Features

- Interactive force-directed network visualisation  
- Chapter-by-chapter animation  
- Dynamic filtering of active characters  
- Support for multiple novels via JSON input  

## 🛠️ Tech Stack

- R  
- Shiny  
- networkD3  
- tidyverse  
- igraph  
- jsonlite  
- purrr  

Each JSON file contains:

- `characters`: character IDs and names  
- `relationships`: chapter-wise interaction data
