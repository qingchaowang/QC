#include <iostream>
#include <string>

using namespace std;

class constructor {
    int a;
    string b;
public:
    constructor(int int_a, string str_b):
        a{int_a},
        b(str_b){
        cout << "constructor is called" << endl;
        cout << "a is "<< int_a << endl;
        cout << "b is "<< b << endl;
    }
};

int main()
{
    constructor c(3, "a");
    return 0;
}
