#include<bits/stdc++.h>
#include<iterator>
using namespace std;

bool wpreference(int n, vector<vector<int> > &preferw, int w, int m1, int m2){
	for(int i = 0; i < n; i++){
		if(preferw[w][i] == m1) return 1;
		if(preferw[w][i] == m2) return 0;
	}
}

int gale_shapley(int n, vector<vector<int> > &preferw, vector<vector<int> > &preferm){
	int num_iter = 0;
	vector<int> partner_woman(n, -1);
	vector<int> man_rejected(n, 0);
	set<int> unpairedm;
	for(int i = 0; i < n; i++){
		unpairedm.insert(i);
	}
	while(unpairedm.size() > 0){
		int man = *unpairedm.begin();
		unpairedm.erase(man);
		num_iter++;
		int woman = preferm[man][man_rejected[man]];
		// if the woman is unpaired
		if(partner_woman[woman] == -1){
			partner_woman[woman] = man;
			man_rejected[man]++;
		}
		else {
			int mpair = partner_woman[woman];
			if(wpreference(n, preferw, woman, man, mpair)){
				partner_woman[woman] = man; // new man is paired to the woman
				unpairedm.insert(mpair);
			}
			else{
				unpairedm.insert(man);
				man_rejected[man]++;
			}
		}
	}
	return num_iter;	
}

vector<int> randomPermutation(int start, int end){
    vector<int> perm;
    for (int i = start; i <= end; ++i) {
        perm.push_back(i);
    }
    random_shuffle(perm.begin(), perm.end());
    return perm;
}

vector<vector<int>> generateRandomMatrix(int N) {
    vector<vector<int>> matrix(N, vector<int>(N));

    for (int i = 0; i < N; ++i) {
        vector<int> permutation = randomPermutation(0, N - 1);
        for (int j = 0; j < N; ++j) {
            matrix[i][j] = permutation[j];
        }
    }

    return matrix;
}

int main(){

    for(int i = 1; i < 1e4; i = i + 1){
	    int ans = 0;
		for(int j = 0; j < 10; j++){
			vector<vector<int> > preferw = generateRandomMatrix(i);	
			vector<vector<int> > preferm = generateRandomMatrix(i);
			ans += gale_shapley(i, preferw, preferm);
		}
		cout << i << " " << ans/10 << endl;
	}
	
	
}
