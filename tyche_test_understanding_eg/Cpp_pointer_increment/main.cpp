#include <iostream>
#include <vector>
using namespace std;

int main()
{
//    cout << "Hello World!" << endl;
    vector<int> a {1, 2, 3, 4, 5, 6};
    auto it = a.begin();
    auto stop = --a.end();
    for (; it!= stop; ++it) {
        cout << *it << endl; }
    cout << *it << endl;
    return 0;
}
