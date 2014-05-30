/*
gcc -o fan fannkuch.c
gcc -o fan2 -O2 fannkuch.c
*/
#define ASMALL unsigned int 
#define SMALL  unsigned int
#define Length 100
   
long fannkuch(n, Perm, Perm1, Zaehl, PermMax)
     SMALL n;
     ASMALL Perm[], Perm1[], Zaehl[], PermMax[];
{
  long BishMax=-1, Spiegelungsanzahl;
  SMALL r, i, k;
  for (i=0; i<n; i++)
    Perm1[i]=i;
  r=n;
  while(1) {
    while (r != 1) {
      Zaehl[r-1]=r;
      r=r-1;
    }
    if (! (Perm1[0] == 0 || (i=n-1, Perm1[i] == i))) {
      for (i = 0; i < n; i = i + 1)
        Perm[i] = Perm1[i];
      Spiegelungsanzahl=0;
      while (!((k=Perm[0]) == 0)) {
        SMALL k2=(k+1)>>1;
        for (i = 0; i < k2; i = i + 1) {
          SMALL temp = Perm[i];
          Perm[i] = Perm[k - i];
          Perm[k - i] = temp;
        }
        Spiegelungsanzahl = Spiegelungsanzahl + 1;
      }
      if (Spiegelungsanzahl > BishMax) {
        BishMax = Spiegelungsanzahl;
        for (i = 0; i < n; i = i + 1)
          PermMax[i] = Perm1[i];
      }
    }
    while(1) {
      if (r == n) return(BishMax);
      { SMALL Perm0;
        Perm0 = Perm1[0];
        i = 0;
        while (i < r) {
          k = i + 1;
          Perm1[i]=Perm1[k];
          i = k;
        }
        Perm1[r]=Perm0;
      }
      if ((Zaehl[r] = Zaehl[r]-1) > 0) break;
      r=r+1;
    }
  }
}

void main()
{ SMALL n;
  long start;
  ASMALL Perm[Length], Perm1[Length], Zaehl[Length], PermMax[Length];
  long BishMax;
  start = clock();
  n = 11;
  BishMax = fannkuch(n, Perm, Perm1, Zaehl, PermMax);
  printf ("%d %d\n", BishMax, clock() - start);
}
