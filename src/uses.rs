use crate::{procedure::Procedure, value::ValueId};

pub struct Uses {
    pub counts: Vec<Counts>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Counts {
    pub num_uses: usize,
    pub num_using_instructions: usize,
    pub users: Vec<ValueId>,
}

impl Uses {
    pub fn new(proc: &Procedure) -> Uses {
        let mut children = Vec::with_capacity(64);

        let mut counts = vec![
            Counts {
                num_uses: 0,
                num_using_instructions: 0,
                users: Vec::new(),
            };
            proc.values.size()
        ];

        for value in (0..proc.values.size()).map(ValueId) {
            if proc.values.at(value).is_none() {
                continue;
            }
            children.truncate(0);

            for child in proc.value(value).children.iter() {
                counts[child.0].num_uses += 1;
                counts[child.0].users.push(value);
                children.push(*child);
            }

            children.sort();

            let mut last = None;

            for child in children.iter().copied() {
                if Some(child) == last {
                    continue;
                }

                counts[child.0].num_using_instructions += 1;

                last = Some(child);
            }
        }

        for count in counts.iter_mut() {
            count.users.sort();
            count.users.dedup();
        }

        Self { counts }
    }

    pub fn uses(&self, value: ValueId) -> &[ValueId] {
        &self.counts[value.0].users
    }

    pub fn num_uses(&self, value: ValueId) -> usize {
        self.counts[value.0].num_uses
    }

    pub fn num_using_instructions(&self, value: ValueId) -> usize {
        self.counts[value.0].num_using_instructions
    }
}
