// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#ifndef __SHARED_H
#define __SHARED_H

YS_MAYBE_UNUSED static void create_seer(Module *m, IdString id, SigSpec inp, SigSpec out, int offset)
{
	log_assert(inp.size() == out.size());

	if (offset == 0) {
		m->connect(out, inp);
		return;
	}

	auto s = m->addCell(id, ID(SEER));
	s->setPort(ID::A, inp);
	s->setPort(ID::Y, out);
	s->setParam(ID(WIDTH), inp.size());
	s->setParam(ID(OFFSET), offset);
}

struct seer_trees_database_t {
	SigMap &sigmap;

	struct upstream_t {
		SigBit bit;
		int offset;
	};

	struct driver_t {
		int bitpos;
		Cell *cell;
	};

	dict<SigBit, driver_t> drivers;
	dict<SigBit, upstream_t> sources;
	dict<SigBit, SigPool> leaves;
	SigPool covered_bits;
	SigPool roots;

	seer_trees_database_t(SigMap &sigmap) : sigmap(sigmap) {}

	void register_cell(Cell *cell)
	{
		if (cell->type != ID(SEER))
			return;

		auto in = cell->getPort(ID(A));
		auto out = sigmap(cell->getPort(ID(Y)));

		int offset = cell->getParam(ID(OFFSET)).as_int(true);
		for (int i = 0; i < in.size(); i++) {
			covered_bits.add(out[i]);
			drivers[out[i]] = driver_t{ i, cell };
			sources[out[i]] = { .bit = in[i], .offset = offset };
		}
	}

	void traverse_sources()
	{
		for (auto bit : covered_bits.export_all()) {
			upstream_t up = sources[bit];
			while (sources.count(sigmap(up.bit))) {
				auto upup = sources[sigmap(up.bit)];
				log_assert(sigmap(upup.bit) != bit); // TODO
				up = {
					.bit = upup.bit,
					.offset = up.offset + upup.offset,
				};
			}
			sources[bit] = up;
			roots.add(up.bit);
			leaves[sigmap(up.bit)].add(bit);
		}
	}

	upstream_t lookup(SigBit bit)
	{
		auto mapped_bit = sigmap(bit);
		if (!sources.count(mapped_bit))
			return upstream_t{ bit, 0 };
		else
			return sources[mapped_bit];
	}

	SigSpec wide_lookup(SigSpec sig)
	{
		if (sig.size() == 0)
			return SigSpec();

		int offset = lookup(sig[0]).offset;
		SigSpec ret;
		for (auto bit : sig) {
			auto up = lookup(bit);
			ret.append(up.bit);
			log_assert(up.offset == offset);
		}
		return ret;
	}

	SigSpec lookup_leaves(SigBit bit)
	{
		log_assert(lookup(bit).bit == bit);
		auto ret = leaves[sigmap(bit)];
		ret.add(bit);
		return ret.export_all();
	}
};

#endif // include guard __SHARED_H
