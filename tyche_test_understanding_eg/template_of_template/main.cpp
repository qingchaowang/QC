#include <iostream>

using namespace std;

template <class T>
class B {

};

template <class S, template<class> class T>
class D {

};



int main()
{
    using b_int = B<int>;
    D<int, B> C;
    cout << "Hello World!" << endl;
    return 0;
}
