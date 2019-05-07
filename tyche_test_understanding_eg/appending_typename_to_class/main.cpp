#include <iostream>
#include <vector>
#include <tuple>

using namespace std;

template <class Typelist, class X>
class append;

template <template <class...> class Typelist, class X, class... T>
class append<Typelist<T...>, X> {
public:
    using type = Typelist<T..., X>;
};

template <class Typelist, class T>
using append_t = typename append<Typelist, T>::type;

int main()
{
    append_t<tuple<int>, double> test;

    return 0;
}
