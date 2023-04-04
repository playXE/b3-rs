use crate::{procedure::Procedure, value::ValueId};

pub struct UseCounts {
    pub counts: Vec<Counts>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Counts {
    pub num_uses: usize,
    pub num_using_instructions: usize,
}

impl UseCounts {
    pub fn new(proc: &Procedure) -> UseCounts {
        let mut children = Vec::with_capacity(64);

        let mut counts = vec![
            Counts {
                num_uses: 0,
                num_using_instructions: 0
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

        Self { counts }
    }

    pub fn num_uses(&self, value: ValueId) -> usize {
        self.counts[value.0].num_uses
    }

    pub fn num_using_instructions(&self, value: ValueId) -> usize {
        self.counts[value.0].num_using_instructions
    }
}
