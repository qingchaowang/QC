#include <iostream>

using namespace std;

class test{
    int i;
    char chr;
public:
    constexpr auto func_test() {return i;}
    constexpr auto func_test() const {return chr;}

    constexpr test(int j, char k):
        i(j),
        chr(k) {cout << func_test() << endl;}


};


int main()
{
    const test t(1, 'a');
    return 0;
}
