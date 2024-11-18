use std::{path::PathBuf, sync::Arc};

use ls_types::{File, LintProgress, LintState, Project};
use tokio::sync::RwLock;

pub mod ls;
pub mod ls_types;

pub mod powerbuilder_proto {
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.rs"));
    include!(concat!(env!("OUT_DIR"), "/protobuf/powerbuilder.serde.rs"));
}

pub async fn add_file(
    proj: Arc<RwLock<Project>>,
    path: &PathBuf,
    lint_progress: LintProgress,
) -> anyhow::Result<()> {
    if !proj.read().await.files.contains_key(path) {
        proj.write()
            .await
            .files
            .insert(path.clone(), RwLock::new(File::new(path.clone())?));
    }

    let proj_r = proj.read().await;
    let _file_lock = proj_r.files.get(path).unwrap();
    let mut current_progress = _file_lock.read().await.top_levels.get_progress();

    while current_progress < lint_progress {
        match current_progress.next() {
            Some(progress) => {
                let mut file = _file_lock.write().await;
                LintState::new(proj.clone(), ls_types::MaybeMut::Mut(&mut file))
                    .lint_file()
                    .await;
                current_progress = progress;
            }
            None => break,
        }
    }

    // for diagnostic in &_file_lock.read().await.diagnostics {
    //     println!("{} - {}", diagnostic.range, diagnostic.message)
    // }

    Ok(())
}

