# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from .utils import product
from . import ast


class Shape:
    def __init__(self, *dims, signed=False):
        self._dims = tuple(dims)
        self._signed = signed
        assert self.ndims > 0

    @property
    def ndims(self):
        return len(self._dims)

    @property
    def dims(self):
        return self._dims

    @property
    def signed(self):
        return self._signed

    @property
    def bitlen(self):
        return product(*self._dims)

    @property
    def arraydims(self):
        return self._dims[:-1]

    @property
    def finaldim(self):
        return self._dims[-1]

    @property
    def element(self):
        return self.drop_dims(self.ndims - 1)

    @property
    def signed_finaldim(self):
        return self._dims[-1] + (1 if not self.signed else 0)

    @property
    def signed_extend(self):
        return Shape(*self._dims[:-1], self.signed_finaldim, signed=True)

    @classmethod
    def common_single(self, a, b, force_signed=False):
        assert a.is_single_dim and b.is_single_dim
        if a.signed or b.signed or force_signed:
            return Shape(max(a.signed_finaldim, b.signed_finaldim), signed=True)
        else:
            return Shape(max(a.finaldim, b.finaldim), signed=False)

    @classmethod
    def common(self, a, b):
        if a.dims[:-1] != b.dims[:-1]:
            raise ast.BadInput(f"shapes {a!r}, {b!r} cannot be combined")
        return self.common_single(a.drop_dims(a.ndims - 1),
                                  b.drop_dims(b.ndims - 1)).prepend_dims(a.dims[:-1])

    @classmethod
    def cast_to_common(self, a, b):
        common_shape = self.common(a.shape, b.shape)
        return a.cast(common_shape), b.cast(common_shape)

    @classmethod
    def common_signed(self, a, b):
        if a.dims[:-1] != b.dims[:-1]:
            raise ast.BadInput(f"shapes {a!r}, {b!r} cannot be combined")
        return self.common_single(a.drop_dims(a.ndims - 1),
                                  b.drop_dims(b.ndims - 1),
                                  force_signed=True).prepend_dims(a.dims[:-1])

    @classmethod
    def common(self, a, b):
        if a.dims[:-1] != b.dims[:-1]:
            raise ast.BadInput(f"shapes {a!r}, {b!r} cannot be combined")
        return self.common_single(a.drop_dims(a.ndims - 1),
                                  b.drop_dims(b.ndims - 1)).prepend_dims(a.dims[:-1])

    def drop_dims(self, ndims):
        return Shape(*self._dims[ndims:], signed=self.signed)

    def prepend_dims(self, dims):
        return Shape(*(dims + self._dims), signed=self.signed)

    @property
    def is_single_dim(self):
        return len(self._dims) == 1

    @property
    def min_value(self):
        assert self.is_single_dim
        return -2**(self.finaldim - 1) if self.signed else 0

    @property
    def max_value(self):
        assert self.is_single_dim
        return 2**(self.signed_finaldim - 1) - 1

    def __getitem__(self, *keys):
        return self._dims.__getitem__(*keys)

    def __len__(self):
        return len(self._dims)

    def __iter__(self):
        return iter(self._dims)

    def __repr__(self):
        return "[{}]{}".format(
            ",".join(map(str, self._dims)),
            " signed" if self._signed else ""
        )
