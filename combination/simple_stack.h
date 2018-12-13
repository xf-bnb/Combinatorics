#pragma once



class _simple_fix_stack
{
public:

	_simple_fix_stack(unsigned int n) : _size(n), _index(0) { _stack = new unsigned int[_size]; }
	~_simple_fix_stack() { delete[] _stack; }

	void push(const unsigned int& val) { _stack[_index++] = val; }
	unsigned int pop() { return _stack[--_index]; }

	bool is_full() const { return (_index == _size); }
	bool is_empty() const { return (_index == 0); }

	const unsigned int* data() const { return _stack; }
	unsigned int index() const { return _index; }

	bool existing(const unsigned int& val) const
	{
		for (unsigned int _pos = 0; _pos < _index; ++_pos)
			if (val == _pos[_stack])
				return true;

		return false;
	}

private:

	unsigned int* _stack;
	unsigned int _index;
	const unsigned int _size;

};
