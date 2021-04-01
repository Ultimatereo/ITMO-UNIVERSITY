#include <bits/stdc++.h>
using namespace std;

const int INF = 1e9 + 228;
signed main() {
    int n, s;
    cin >> n >> s;

    vector<int> w(n);
    for (int i = 0; i < n; i++) {
        cin >> w[i];
    }

    vector<pair<int, int>> dp(1 << n, {INF, INF});
    vector<int> prev(1 << n);
    dp[0] = {0, 0};
    for (int x = 0; x < (1 << n); x++) {
        for (int i = 0; i < n; i++) {
            if (!(x & (1 << i))) {
                int ff = dp[x].first;
                int ss = dp[x].second;

                if (w[i] + dp[x].second > s) {
                    ff++;
                    ss = w[i];
                } else {
                    ss += w[i];
                }

                if ((dp[x | (1 << i)].first == ff && dp[x | (1 << i)].second > ss) || dp[x | (1 << i)].first > ff) {
                    dp[x | (1 << i)] = {ff, ss};
                    prev[x | (1 << i)] = x;
                }
            }
        }
    }
    cout << dp[(1 << n) - 1].first + (dp[(1 << n) - 1].second > 0) << "\n";

    vector<vector<double>> ans;
    for (int x = (1 << n) - 1; x > 0;) {
        int cnt = dp[x].first;
        vector<double> tmp;
        while (x > 0 && cnt == dp[x].first) {
            tmp.push_back(log2(x ^ prev[x]) + 1);
            x = prev[x];
        }

        ans.push_back(tmp);
    }

    for (auto & i : ans) {
        cout << i.size();
        for (double j : i) {
            cout << " " << j;
        }
        cout << "\n";
    }

    return 0;
}
