#include <iostream>

using namespace std;

int main()
{
    int x = 42;
    new(&x) int(52);
    cout << x << endl;
    return 0;
}
