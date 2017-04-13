# Summary

This project implements Alvin E. Roth's matching algorithm as described in:

```Roth, Alvin E. (2015-06-02). Who Gets What — and Why: The New Economics of Matchmaking and Market Design (pp. 141-142). Houghton Mifflin Harcourt. Kindle Edition.``` 

# Instructions

Clone the project. Run SBT. To run with the sample files, type the following at the SBT prompt:

```
run "src/main/resources/candidates1.csv" "src/main/resources/employers1.csv" "/path/to/output/file.txt"```
run "src/main/resources/candidates2.csv" "src/main/resources/employers2.csv" "/path/to/output/file.txt"```
run "src/main/resources/candidates3.csv" "src/main/resources/employers3.csv" "/path/to/output/file.txt"```
```

The first file is the candidates data file, in the form:

```
candidate id, id of most prefered employer, id of next most prefered employer, ...
```

e.g.

```
1,1,3,2
2,1,2,3
3,2,3,1
```

The second file is the employers data file, in the form:

```
employer id, available positions, id of most prefered candidate, id of next most prefered candidate, ...
```

e.g.

```
1,1,3,2,1
2,1,2,3,1
3,1,3,2,1
```

Files with these data are in the ```src/main/resources/``` directory.

For the above data, the results are:

```
1 -> 3
2 -> 1
3 -> 2
```

Constraints to observe for data files: 

1. No duplicate ids each each file 
2. Preference ids should be unique for each row of data