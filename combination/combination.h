#pragma once


template<typename _FuncT>
unsigned int Combination(unsigned int _size, unsigned int _n, _FuncT pfunc)
{
	if (_n > _size)
		_n = _size;

	if (_n == 0)
		return pfunc(nullptr, 0) ? 1 : 0;

	unsigned int _Count = 0;
	const unsigned int _x = _size - _n;

	_simple_fix_stack _sfs{ _n };

	for (unsigned int _pos = 0; ; ++_pos)
	{
		_sfs.push(_pos);

		while (_sfs.is_full())
		{
			if (pfunc(_sfs.data(), _sfs.index()))
				++_Count;

			for (_pos = _size; (_x + _sfs.index()) <= _pos && !_sfs.is_empty(); )
			{
				_pos = _sfs.pop() + 1;

				if (_pos <= (_x + _sfs.index()))
				{
					_sfs.push(_pos);
					break;
				}
			}
		}

		if (_sfs.is_empty())
			break;
	}

	return _Count;
}


template<typename _FuncT>
unsigned int Combination(_FuncT pFunc, unsigned int m, const unsigned int* _Src, unsigned int n)
{
    unsigned int _Top = 0, _Pos = 0, _Sum = 0;
    unsigned int _Stack[100] = { 0 }, _Count[100] = { 0 };

    for (unsigned int i = 0;; ++_Pos)
    {
        for (; _Pos < n; ++_Pos)
        {
            if (_Src[_Pos])
            {
                _Stack[_Top] = _Pos;
                ++_Count[_Top];
                ++_Sum;
                ++_Top;

            _Full:          if (_Sum == m)
            {
                ++i;
                pFunc(_Stack, _Count, _Top, n);

                --_Top;
                _Sum -= _Count[_Top];
                _Count[_Top] = 0;
            }
            }
        }

        if (!_Top) return i;

        _Pos = _Stack[--_Top];

        if (_Count[_Top] < _Src[_Pos])
        {
            ++_Count[_Top];
            ++_Sum;
            ++_Top;

            goto _Full;
        }
        else
        {
            _Sum -= _Count[_Top];
            _Count[_Top] = 0;
        }
    }
}
