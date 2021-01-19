// Counts Palindromic Subsequence in a given String
#include<iostream>
#include<cstring>
using namespace std;

// Function return the total palindromic subsequence
int countPS(string str)
{
	int N = str.length();

	// create a 2D array to store the count of palindromic
	// subsequence
	int cps[N+1][N+1];
	memset(cps, 0 ,sizeof(cps));

	// palindromic subsequence of length 1
	// for (int i=0; i<N; i++) {
	//     for (int j=0; j<N; j++) {
	//         if(i==j){
	//             cps[i][i] = 1;
	//         }
	//         else{
	//             cps[i][j]=0;
	//         }
  //   	}
	// }
	for (int i=0; i<N; i++)
		cps[i][i] = 1;


	// check subsequence of length L is palindrome or not
	for (int L=2; L<=N; L++)
	{
		for (int i=0; i<N; i++)
		{
			int k = L+i-1;
			cout<<i<<k<<"\n";
			if (str[i] == str[k])
				cps[i][k] = cps[i][k-1] +
							cps[i+1][k] + 1;
			else
				cps[i][k] = cps[i][k-1] +
							cps[i+1][k] -
							cps[i+1][k-1];



			for (int i = 0; i < N+1; i++) {
	        for (int j = 0; j < N+1; j++) {
	            // cps[i][j] = '.';
	            printf("%d ", cps[i][j]);
	        }
	        printf("\n");
	    }
			printf("\n");
		}
	}

	// return total palindromic subsequence
	return cps[0][N-1];
}

// Driver program
int main()
{
	string str = "abcb";
	cout << countPS(str) << endl;
	return 0;
}
