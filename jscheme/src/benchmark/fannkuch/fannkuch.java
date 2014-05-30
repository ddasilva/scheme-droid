/**
see
http://openmap.bbn.com/~kanderso/performance/postscript/fannkuch.ps
*/
import java.util.Date;

class fannkuch {
  static int Length = 100;
   
  static long iterate(int n,int Perm[],int Perm1[],int Zaehl[],int PermMax[]) {
    long BishMax=-1, Spiegelungsanzahl;
    int r, i, k;
    for (i=0; i<n; i++)
      Perm1[i]=i;
    r=n;
    while(true) {
      while (r != 1) {
	Zaehl[r-1]=r;
	r=r-1;
      }
      //if (! (Perm1[0] == 0 || (i=n-1, Perm1[i] == i))) 
      if (Perm1[0] != 0) {
	i=n-1;
	if (Perm1[i] != i) {
	  for (i = 0; i < n; i = i + 1)
	    Perm[i] = Perm1[i];
	  Spiegelungsanzahl=0;
	  while (!((k=Perm[0]) == 0)) {
	    int k2=(k+1)>>1;
	    for (i = 0; i < k2; i = i + 1) {
	      int temp = Perm[i];
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
      }
      while(true) {
	if (r == n) return(BishMax);
	{ 
	  int Perm0;
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

  public static void main(String argv[]) {
    int n = 11;
    if (argv.length == 1) n = Integer.parseInt(argv[0]);
    int Perm[] = new int[Length];
    int Perm1[]= new int[Length];
    int Zaehl[]= new int[Length];
    int PermMax[]= new int[Length];
    long BishMax;

    String vminfo = System.getProperty("java.vm.info");
    String vmv = System.getProperty("java.vm.version");
    String os = System.getProperty("os.name");
    String osv = System.getProperty("os.version");
    System.out.println("JDK "+vmv+" ("+vminfo+") "+os+" ("+osv+")");

    /* Let hotspot notice it. */
    BishMax = iterate(n, Perm, Perm1, Zaehl, PermMax);
    BishMax = iterate(n, Perm, Perm1, Zaehl, PermMax);


    long d1 = System.currentTimeMillis();
    BishMax = iterate(n, Perm, Perm1, Zaehl, PermMax);
    long d2 = System.currentTimeMillis();
    long elapsed = d2 - d1;
    System.out.println(BishMax+" "+elapsed);
  }
}
