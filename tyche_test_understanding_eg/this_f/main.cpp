#include <iostream>

using namespace std;

class test {
public:
    int f(int a, int b){return a + b;}
    int g(int a, int b){
        auto h = 3;
        return (this-> *h)(a, b);
    }
};

int main()
{
    test t;
    cout << t.g(3, 4) << endl;
    return 0;
}
